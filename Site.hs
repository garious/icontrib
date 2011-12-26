{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Site where

import Control.Monad.Trans                   ( liftIO, lift )
import Data.Acid                             ( AcidState )
import Char                                  ( chr )
import Control.Monad.Error                   ( runErrorT )
import Control.Monad                         ( liftM )

import qualified Data.ByteString.Lazy        as B
import qualified Account                     as A
import qualified Codec.Binary.Url            as Url
import qualified Text.JSON                   as JS
import qualified ServerError                 as SE
import Happstack.Lite

checkUser ::  AcidState A.Database -> ServerPart Response
checkUser db = do 
   liftIO $ putStrLn "checkUser" 
   method GET
   rv <- runErrorT $ do cookie <- lift $ liftM Url.decode $ lookCookieValue "token"
                        token <- SE.checkMaybe SE.CookieDecode $ liftM B.pack $ cookie 
                        uid <- A.cookieToUser db token
                        let msg = "Thanks for comming back " ++ (toS uid)
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


