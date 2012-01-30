{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module Site where

import Control.Monad.Trans                   ( liftIO, lift )
import Data.Acid                             ( AcidState )
import Control.Monad                         ( liftM )
import Char                                  ( chr )
import Control.Monad.Error                   ( runErrorT, ErrorT, throwError )
import Happstack.Server.Monads               ( ServerPartT )
--import Control.Applicative                   ( (<|>) )
import qualified Data.ByteString.Lazy        as B
import qualified Codec.Binary.Url            as Url
import qualified Text.JSON                   as JS
import qualified ServerError                 as SE
import qualified Account                     as A
import qualified CharityInfo                 as C
import qualified UserInfo                    as U
import qualified JsWidget                    as JSW
import qualified Network.HTTP                as HTTP
import Happstack.Server(askRq, rqUri, lookPairs)
import Happstack.Lite


data Site = Site { userAccounts ::  AcidState A.Database
                 , charityInfo :: AcidState C.Database
                 , userInfo :: AcidState U.Database
                 }

site :: Site -> ServerPart Response
site st = msum [ 
      dir "auth"    (authServices st)
    , dir "donor"   (donorServices st)
    , dir "charity" (charityServices st)
    , dir "mirror" $ dir "google" $ dir "jsapi" (redirect (HTTP.getRequest "https://www.google.com/jsapi"))
    , JSW.widget root []
    , fileServer root
    ]
  where
    root = "public"

authServices:: Site -> ServerPart Response
authServices st = msum [ 
      dir "login"  (post    (loginUser  "auth" (userAccounts st)))
    , dir "add"    (post    (addUser    "auth" (userAccounts st)))
    , dir "logout" (get     (check >>= logOut (userAccounts st)))
    , dir "check"  (get     check)
    ]
    where
        check = (checkUser "auth" (userAccounts st))

donorServices:: Site -> ServerPart Response
donorServices st = msum [ 
      dir "update"               (post  (check >>= (withBody (U.updateInfo (userInfo st)))))
    , dir "get"                  (get   (check >>= (U.lookupInfo (userInfo st))))
    , dir "mostInfluential.json" (get   (U.mostInfluential (userInfo st)))
    , dir "ls"                   (get   (liftIO $ U.list (userInfo st)))
    ]
    where
        check = (checkUser "auth" (userAccounts st))


charityServices :: Site -> ServerPart Response
charityServices st = msum [ 
      dir "update" (post (check >>= (withBody (C.updateInfo (charityInfo st)))))
    , dir "get"    (get  (check >>= (C.lookupInfo (charityInfo st))))
    ]
    where
        check = (checkUser "auth" (userAccounts st))

withBody :: JS.JSON t => (t1 -> t -> IO b) -> t1 -> ErrorT SE.ServerError (ServerPartT IO) b 
withBody ff uid = do
    bd <- getBody
    liftIO $ ff uid bd

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
   rv <- runErrorT page
   rsp $ rv 

post :: (Show a, JS.JSON a) => ErrorT SE.ServerError (ServerPartT IO) a -> ServerPartT IO Response
post page = do 
   method POST
   rq <- askRq
   liftIO $ print ("post"::String, (rqUri rq))
   rv <- (runErrorT page)
   rsp $ rv

homePage :: ServerPart Response
homePage = serveDirectory DisableBrowsing ["index.html"] "public"

fileServer :: FilePath -> ServerPart Response
fileServer dd = do 
    addHeaderM "Pragma" "no-cache"
    serveDirectory DisableBrowsing [] dd

logOut :: AcidState A.Database ->  A.UserID -> ErrorT SE.ServerError (ServerPartT IO) ()
logOut db uid = do 
    liftIO $ putStrLn $ "logout: " ++ (toS uid)
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

