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
      dir "user"    (userServices st)
    , dir "charity" (charityServices st)
    , dir "mirror" $ dir "google" $ dir "jsapi" (redirect (HTTP.getRequest "https://www.google.com/jsapi"))
    , JSW.widget root []
    , fileServer root
    ]
  where
    root = "public"

userServices :: Site -> ServerPart Response
userServices st = msum [ 
      dir "login"          (post    (loginUser  "user" (userAccounts st)))
    , dir "check"          (get     (checkUser  "user" (userAccounts st)))
    , dir "logout"         (get     (logOut     "user" (userAccounts st)))
    ]

charityServices :: Site -> ServerPart Response
charityServices st = msum [ 
      dir "login"      (post (loginUser  "charity" (charityAccounts st)))
    , dir "check"      (get  (checkUser  "charity" (charityAccounts st)))
    , dir "logout"     (get  (logOut     "charity" (charityAccounts st)))
    , dir "add"        (post (addUser    "charity" (charityAccounts st)))
    , dir "getInfo"    (get  (check' >>= (C.lookupInfo (charityInfo st))))
    , dir "updateInfo" (post (check' >>= (updateInfo (charityInfo st))))
    ]
    where
        check' = (checkUser "charity" (charityAccounts st))

updateInfo :: AcidState C.Database -> C.CharityID -> ErrorT SE.ServerError (ServerPartT IO) ()
updateInfo db uid = do
    vv <- getBody
    liftIO $ C.updateInfo db uid vv

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

logOut :: String -> AcidState A.Database -> ErrorT SE.ServerError (ServerPartT IO) ()
logOut name db = do 
    liftIO $ putStrLn "logout" 
    cookie <- lift $ liftM Url.decode $ lookCookieValue name
    liftIO $ print cookie
    token <- SE.checkMaybe SE.CookieDecode $ liftM B.pack $ cookie 
    uid <- A.cookieToUser db token
    liftIO $ A.clearUserCookie db uid
    liftIO $ putStrLn "cleared cookies" 

checkUser :: String -> AcidState A.Database -> ErrorT SE.ServerError (ServerPartT IO) A.UserID
checkUser name db = do 
   liftIO $ putStrLn "check" 
   cookie <- lift $ liftM Url.decode $ lookCookieValue name 
   liftIO $ print cookie
   token <- SE.checkMaybe SE.CookieDecode $ liftM B.pack $ cookie 
   uid <- A.cookieToUser db token
   return uid

loginUser :: String -> AcidState A.Database -> ErrorT SE.ServerError (ServerPartT IO) A.UserID
loginUser name db = do 
   liftIO $ putStrLn "login" 
   (A.UserLogin uid pwd) <- getBody
   token <- A.loginToCookie db (uid) (pwd)
   liftIO $ putStrLn (show uid)
   let cookie = mkCookie name (Url.encode (B.unpack token))
   lift $ addCookies [(Session, cookie)]
   return $ uid

addUser :: String -> AcidState A.Database -> ErrorT SE.ServerError (ServerPartT IO) A.UserID
addUser name db = do 
   liftIO $ putStrLn "add" 
   (A.UserLogin uid pwd) <- getBody
   A.addUser db (uid) (pwd)
   loginUser name db
   
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

