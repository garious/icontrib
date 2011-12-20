{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Server where

import Data.Text (Text)
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, type_, value)
import Control.Monad.Error ( runErrorT )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Account as Account
import Data.Acid.Memory                      ( openMemoryState )
import Data.Acid                             ( AcidState )
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL

addUser ::  AcidState Account.Database -> ServerPart Response
addUser db = msum [ viewForm, processForm ]
  where
    viewForm :: ServerPart Response
    viewForm =
        do method GET
           ok $ template "addUser" $
              form ! action "/addUser" ! enctype "multipart/form-data" ! A.method "POST" $ do
                label ! A.for "msg" $ "AddUser "
                input ! type_ "text" ! A.id "name" ! name "name"
                input ! type_ "submit" ! value "name:"
                input ! type_ "text" ! A.id "password" ! name "password"
                input ! type_ "submit" ! value "password:"

    processForm :: ServerPart Response
    processForm = do 
         method POST
         uid <- lookBS "name"
         pwd <- lookBS "password"
         rv <- runErrorT $ do { Account.rethrowIO $ Account.addUser db (toS uid) (toS pwd)
                              ; return $ H.p "addUser ok"
                              }
         rsp rv

rsp :: Either Account.AccountError Html -> ServerPart Response 
rsp (Left ee) = internalServerError $ template "form" $ H.p (toHtml $ show ee)                            
rsp (Right msg) = ok $ template "form" $ msg

toS :: BL.ByteString -> BS.ByteString
toS = BS.concat . BL.toChunks

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.title (toHtml title)
    H.body $ do
      body
      p $ a ! href "/" $ "back home"

test :: IO ()
test = do
   db <- openMemoryState Account.empty
   serve Nothing (addUser db)
