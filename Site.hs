{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module Site where
import Control.Monad.Trans                   ( liftIO, lift )
import Data.Acid                             ( AcidState )
import Char                                  ( chr )
import Control.Monad.Error                   ( runErrorT, ErrorT )
import Control.Monad                         ( liftM )


import qualified Data.ByteString.Lazy        as B
import qualified Codec.Binary.Url            as Url
import qualified Text.JSON                   as JS
import qualified ServerError                 as SE

import qualified Account                     as A
import qualified CharityInfo                 as C

import Happstack.Server.Monads
import Happstack.Lite

data Site = Site { userAccounts ::  AcidState A.Database
                 , charityAccounts ::  AcidState A.Database
                 , charityInfo :: AcidState C.Database 
                 }

site :: Site -> ServerPart Response
site st = msum [ 
      dir "get_user"            (post    (getUser (userAccounts st)))
    , dir "check_user"          (get     (checkUser (userAccounts st)))
    , dir "get_charity"         (post    (getUser (charityAccounts st)))
    , dir "check_charity"       (get     (checkUser (charityAccounts st)))
    , dir "get_charity_info"    (get     (getInfo (charityAccounts st) (charityInfo st)))
    , dir "update_charity_info" (post    (postInfo (charityAccounts st) (charityInfo st)))
    , homePage
    ]

get :: (JS.JSON e, JS.JSON a) => ErrorT e (ServerPartT IO) a -> ServerPartT IO Response
get page = do 
   method GET
   rv <- runErrorT $ page
   rsp $ rv

post :: (JS.JSON e, JS.JSON a) => ErrorT e (ServerPartT IO) a -> ServerPartT IO Response
post page = do 
   method POST
   rv <- runErrorT $ page 
   rsp $ rv

homePage :: ServerPart Response
homePage = serveDirectory DisableBrowsing ["index.html"] "public"

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
   token <- SE.checkMaybe SE.CookieDecode $ liftM B.pack $ cookie 
   uid <- A.cookieToUser db token
   let msg = "Thanks for coming back " ++ (toS uid)
   liftIO $ putStrLn msg 
   return $ JS.toJSString msg

newUser :: AcidState A.Database -> ErrorT SE.ServerError (ServerPartT IO) JS.JSString
newUser db = do 
   uid <- lift $ lookBS "email"
   pwd <- lift $ lookBS "password"
   liftIO $ putStrLn "newUser" 
   A.addUser db (uid) (pwd)
   token <- A.loginToCookie db (uid) (pwd)
   let msg = "Thank you for registering " ++ (toS uid)
   liftIO $ putStrLn msg 
   let cookie = mkCookie "token" (Url.encode (B.unpack token))
   lift $ addCookies [(Session, cookie)]
   return $ JS.toJSString msg

loginUser :: AcidState A.Database -> ErrorT SE.ServerError (ServerPartT IO) JS.JSString
loginUser db = do 
   uid <- lift $ lookBS "email"
   pwd <- lift $ lookBS "password"
   liftIO $ putStrLn "loginUser" 
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

rsp :: JS.JSON a => a -> ServerPart Response
rsp msg = ok $ toResponse $ JS.encode msg

toS :: B.ByteString -> String
toS ss = map (chr . fromIntegral) $ B.unpack ss

