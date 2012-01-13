{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module Site where

import Control.Monad.Trans                   ( liftIO, lift )
import Data.Acid                             ( AcidState )
import Control.Monad                         ( liftM )
import Char                                  ( chr )
import Control.Monad.Error                   ( runErrorT, ErrorT, throwError )
import Happstack.Server.Monads               ( ServerPartT )
import Control.Applicative                   ( (<|>) )
import qualified Data.ByteString.Lazy        as B
import qualified Codec.Binary.Url            as Url
import qualified Text.JSON                   as JS
import qualified ServerError                 as SE
import qualified Account                     as A
import qualified CharityInfo                 as C
import qualified JsWidget                    as JSW
import qualified Network.HTTP                as HTTP
import Happstack.Server(askRq, rqUri, lookPairs)
import Happstack.Lite


data Site = Site { userAccounts ::  AcidState A.Database
                 , charityAccounts ::  AcidState A.Database
                 , charityInfo :: AcidState C.Database 
                 }

site :: Site -> ServerPart Response
site st = msum [ 
      dir "login_user"          (post    (getUser (userAccounts st)))
    , dir "check_user"          (get     (checkUser (userAccounts st)))
    , dir "login_charity"       (post    (getUser (charityAccounts st)))
    , dir "check_charity"       (get     (checkUser (charityAccounts st)))
    , dir "get_charity_info"    (get     (getInfo (charityAccounts st) (charityInfo st)))
    , dir "update_charity_info" (post    (postInfo (charityAccounts st) (charityInfo st)))
    , dir "mirror" $ dir "google" $ dir "jsapi" (redirect (HTTP.getRequest "https://www.google.com/jsapi"))
    , JSW.widget root []
    , fileServer root
    ]
  where
    root = "public"

redirect ::  HTTP.Request_String -> ServerPart Response
redirect req = do
   hrsp <- liftIO $ liftM check $ (HTTP.simpleHTTP req)
   setResponseCode $ fromCode $ HTTP.rspCode hrsp
   mapM_ (\ (HTTP.Header name val) -> setHeaderM (show name) val) $ HTTP.rspHeaders hrsp 
   return $ toResponse $ HTTP.rspBody hrsp
   where
      fromCode (x,y,z) = x * 100 + y * 10 + z
      check (Left err)  = error (show err) 
      check (Right rr)  = rr

get :: (Show a, JS.JSON a) => ErrorT SE.ServerError (ServerPartT IO) a -> ServerPartT IO Response
get page = do 
   method GET
   rq <- askRq
   liftIO $ print ("get"::String, (rqUri rq))
   let err = return (Left (SE.InternalError))
   rv <- ((runErrorT page) <|> err)
   rsp $ rv

post :: (Show a, JS.JSON a) => ErrorT SE.ServerError (ServerPartT IO) a -> ServerPartT IO Response
post page = do 
   method POST
   rq <- askRq
   liftIO $ print rq
   let err = return (Left (SE.InternalError))
   rv <- ((runErrorT page) <|> err)
   rsp $ rv

homePage :: ServerPart Response
homePage = serveDirectory DisableBrowsing ["index.html"] "public"

fileServer :: FilePath -> ServerPart Response
fileServer dd = do 
    addHeaderM "Pragma" "no-cache"
    serveDirectory DisableBrowsing [] dd

getInfo :: AcidState A.Database -> AcidState C.Database -> ErrorT SE.ServerError (ServerPartT IO) C.CharityInfo
getInfo userdb infodb = do 
   liftIO $ putStrLn "getInfo" 
   cookie <- lift $ liftM Url.decode $ lookCookieValue "token"
   token <- SE.checkMaybe SE.CookieDecode $ liftM B.pack $ cookie 
   uid <- A.cookieToUser userdb token
   C.lookupInfo infodb uid

postInfo :: AcidState A.Database -> AcidState C.Database -> ErrorT SE.ServerError (ServerPartT IO) ()
postInfo userdb infodb = do 
   liftIO $ putStrLn "postInfo" 
   jdata <- lift $ lookBS "data"
   cookie <- lift $ liftM Url.decode $ lookCookieValue "token"
   token <- SE.checkMaybe SE.CookieDecode $ liftM B.pack $ cookie 
   uid <- A.cookieToUser userdb token
   C.updateInfo infodb uid (toS jdata)

checkUser :: AcidState A.Database -> ErrorT SE.ServerError (ServerPartT IO) JS.JSString
checkUser db = do 
   liftIO $ putStrLn "checkUser" 
   cookie <- lift $ liftM Url.decode $ lookCookieValue "token"
   liftIO $ print cookie
   token <- SE.checkMaybe SE.CookieDecode $ liftM B.pack $ cookie 
   uid <- A.cookieToUser db token
   let msg = "Thanks for coming back " ++ (toS uid)
   liftIO $ putStrLn msg 
   return $ JS.toJSString msg

newUser :: AcidState A.Database -> ErrorT SE.ServerError (ServerPartT IO) JS.JSString
newUser db = do  
   liftIO $ putStrLn "newUser" 
   (A.UserLogin uid pwd) <- getBody
   A.addUser db (uid) (pwd)
   token <- A.loginToCookie db (uid) (pwd)
   let msg = "Thank you for registering " ++ (toS uid)
   liftIO $ putStrLn msg 
   let cookie = mkCookie "token" (Url.encode (B.unpack token))
   lift $ addCookies [(Session, cookie)]
   return $ JS.toJSString msg


loginUser :: AcidState A.Database -> ErrorT SE.ServerError (ServerPartT IO) JS.JSString
loginUser db = do 
   liftIO $ putStrLn "loginUser" 
   (A.UserLogin uid pwd) <- getBody
   token <- A.loginToCookie db (uid) (pwd)
   let msg = "Welcome Back " ++ (toS uid)
   liftIO $ putStrLn msg 
   let cookie = mkCookie "token" (Url.encode (B.unpack token))
   lift $ addCookies [(Session, cookie)]
   return $ JS.toJSString msg
   
getUser :: AcidState A.Database -> ErrorT SE.ServerError (ServerPartT IO) JS.JSString
getUser db = do
   let ore = SE.catchOnly SE.UserDoesntExist
   loginUser db `ore` newUser db

getBody :: JS.JSON a => ErrorT SE.ServerError (ServerPartT IO) a 
getBody = do   
    liftIO $ putStrLn "getBody"
    bd <- lift $ lookPairs
    let 
            checkResult (JS.Ok a)    = return a
            checkResult (JS.Error ss) = throwError (SE.JSONDecodeError ss)
            decode a = checkResult $ JS.decode a 
            --GIANT FREAKING HACK :)
            --wtf cant i get the request body
            from (name, (Right ss)) = name ++ ss
            from (_, (Left _)) = []
            bd' = concatMap from bd
    liftIO $ print ("body"::String, bd')
    decode $ bd'

rsp :: (Show a, JS.JSON a) => a -> ServerPart Response
rsp msg = do
    liftIO (print msg)
    ok $ toResponse $ JS.encode msg

toS :: B.ByteString -> String
toS ss = map (chr . fromIntegral) $ B.unpack ss

