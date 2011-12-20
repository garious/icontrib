{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Server where

import Data.Text (Text)
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, p, toHtml)
import Text.Blaze.Html5.Attributes (href)
import Control.Monad.Error ( runErrorT, throwError )
import qualified Text.Blaze.Html5 as H
import qualified Account as Account
import Monad                                 ( when )
import Data.Acid                             ( AcidState )
import ServerError

addUser ::  AcidState Account.Database -> ServerPart Response
addUser db = do 
   method POST
   uid <- lookBS "nameS"
   _ <- lookBS "emailS"
   pwd <- lookBS "passwordS"
   pwdre <- lookBS "password_reS"
   _ <- lookBS "typeS"
   rv <- runErrorT $ do { (pwd /= pwdre) `when` throwError PasswordsDontMatch
                        ; Account.rethrowIO $ Account.addUser db (uid) (pwd)
                        ; return $ H.p "addUser ok"
                        }
   rsp rv


rsp :: Either ServerError Html -> ServerPart Response 
rsp (Left ee) = internalServerError $ template "form" $ H.p (toHtml $ show ee)                            
rsp (Right msg) = ok $ template "form" $ msg

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.title (toHtml title)
    H.body $ do
      body
      p $ a ! href "/" $ "back home"

