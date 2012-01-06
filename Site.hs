{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Site where

import Control.Monad.Trans                   ( liftIO, lift )
import Data.Acid                             ( AcidState )
import Control.Monad                         ( liftM )
import Char                                  ( chr )
import Control.Monad.Error                   ( runErrorT )

import qualified Data.ByteString.Lazy        as B
import qualified Account                     as A
import qualified JsWidget                    as JSW
import qualified Codec.Binary.Url            as Url
import qualified Text.JSON                   as JS
import qualified ServerError                 as SE
import qualified Network.HTTP                as HTTP
import Happstack.Lite

site :: AcidState A.Database -> ServerPart Response
site db = msum [ 
      dir "get_user" (getUser db)
    , dir "check_user" (checkUser db)
    , dir "mirror" $ dir "google" $ dir "jsapi" (redirect (HTTP.getRequest "https://www.google.com/jsapi"))
    , JSW.widget root ""
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

fileServer :: FilePath -> ServerPart Response
fileServer = serveDirectory DisableBrowsing []

checkUser ::  AcidState A.Database -> ServerPart Response
checkUser db = do 
   liftIO $ putStrLn "checkUser" 
   method GET
   rv <- runErrorT $ do cookie <- lift $ liftM Url.decode $ lookCookieValue "token"
                        token <- SE.checkMaybe SE.CookieDecode $ liftM B.pack $ cookie 
                        uid <- A.cookieToUser db token
                        let msg = "Thanks for coming back " ++ (toS uid)
                        liftIO $ putStrLn msg 
                        return $ JS.encode $ JS.toJSString msg
   rsp $ rv

getUser ::  AcidState A.Database -> ServerPart Response
getUser db = do 
   method POST
   uid <- lookBS "email"
   pwd <- lookBS "password"
   let ore = SE.catchOnly SE.UserAlreadyExists
   rv <- runErrorT $ do A.addUser db (uid) (pwd)
                        token <- A.loginToCookie db (uid) (pwd)
                        let msg = "Thank you for registering " ++ (toS uid)
                        liftIO $ putStrLn msg 
                        let cookie = mkCookie "token" (Url.encode (B.unpack token))
                        lift $ addCookies [(Session, cookie)]
                        return $ JS.encode $ JS.toJSString msg
               `ore` do token <- A.loginToCookie db (uid) (pwd)
                        let msg = "Welcome Back " ++ (toS uid)
                        liftIO $ putStrLn msg 
                        let cookie = mkCookie "token" (Url.encode (B.unpack token))
                        lift $ addCookies [(Session, cookie)]
                        return $ JS.encode $ JS.toJSString msg

   rsp $ rv


rsp :: (ToMessage a1, Show a) => Either a a1 -> ServerPart Response
rsp (Left ee) = internalServerError $ toResponse $ JS.encode $ JS.toJSString (show ee)
rsp (Right msg) = ok $ toResponse msg

toS :: B.ByteString -> String
toS ss = map (chr . fromIntegral) $ B.unpack ss


