{-# LANGUAGE FlexibleContexts #-}
module Site.Login where

import qualified Log                         as Log
import qualified JSON.UserLogin              as J
import qualified DB.DB                       as DB
import qualified DB.Login                    as L
import qualified Data.Login                  as L
import qualified Happstack.Server            as H
import qualified Codec.Binary.Url            as Url
import qualified Data.ByteString.Lazy        as B

import Site.Utils                            ( getBody )
import Control.Applicative                   ( (<|>) )

import SiteError

loginUser :: DB.Database -> SiteErrorT L.Identity
loginUser db = do
   Log.debugM "login" 
   (J.UserLogin uid pwd) <- getBody
   let ident = (L.toIdentity uid) 
   token <- L.loginToToken db ident (L.toPassword pwd)
   Log.debugM (show uid)
   let cookie = H.mkCookie "auth" (Url.encode (L.tokenUnpack token))
   lift $ H.addCookies [(H.Session, cookie)]
   identity <- L.tokenToIdentity db token
   return $ identity

addUser :: DB.Database -> SiteErrorT L.Identity
addUser db = do 
   Log.debugM "add" 
   (J.UserLogin uid pwd) <- getBody
   L.addIdentity db (L.toIdentity uid) (L.toPassword pwd)
   loginUser db

logOut :: MonadIO m => DB.Database -> L.Identity -> m ()
logOut db uid = do 
    Log.debugM $ "logout: " ++ (show uid)
    liftIO $ L.clearIdentityTokens db uid
    Log.debugM "cleared cookies" 

checkUser :: DB.Database -> SiteErrorT L.Identity
checkUser db = do 
   Log.debugM "check" 
   cookie <- lift $ getCookieValue "auth"
   Log.debugShow cookie
   token <- cookie `justOr` cookieDecodeError
   uid <- L.tokenToIdentity db (L.Token token)
   return uid

getCookieValue :: String -> H.ServerPartT IO (Maybe B.ByteString)
getCookieValue name = do { val <- H.lookCookieValue name
                         ; return $ liftM B.pack $ Url.decode val
                         }
                     <|> return Nothing

cookieDecodeError :: MonadError String m => m a
cookieDecodeError = fail "CookieDecodeError"
