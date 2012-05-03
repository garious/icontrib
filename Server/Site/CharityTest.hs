{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TemplateHaskell #-}
module Site.CharityTest where

import Site.Browser
import qualified Log                         as Log
import Control.Monad.Trans                   ( liftIO )
import Data.Data                             ( Data, Typeable )
import qualified Network.HTTP                as HTTP
import qualified Network.Browser             as HTTP
import qualified JSON.UserLogin              as J
import qualified Data.CharityInfo            as C
import qualified Data.Login                  as L
import qualified Data.Paypal                 as P
import TestUtil


ci :: C.CharityInfo
ci = C.CharityInfo (L.Identity "greg")
                   (C.Ein "10001")
                   "Global Fund for Women"
                   "http://gffw.com"
                    (C.CharityID "gffw")
                    "charity/gffw.jpg"
                    "blah blah blah"
                    (P.Email  "payment@info.com")
                    []

data PartialCI = PartialCI { ein :: String
                           , organizationName :: String
                           , companyWebsite :: String
                           , paymentAddress :: String
                           }
                           deriving (Eq, Ord, Show, Data, Typeable)

mci :: PartialCI
mci = PartialCI ("10001")
                "Global Fund for Women"
                "http://gffw.com"
                "payment@info.com"

addUser :: J.UserLogin ->  HTTP.BrowserAction (HTTP.HandleStream String) (Either String J.UserIdentity)
addUser = post 200 "/auth/add"

update :: C.CharityInfo ->  HTTP.BrowserAction (HTTP.HandleStream String) (Either String ())
update = post 200 "/charity/update"

register :: PartialCI ->  HTTP.BrowserAction (HTTP.HandleStream String) (Either String ())
register = post 200 "/charity/update"

readInfo :: C.CharityID ->  HTTP.BrowserAction (HTTP.HandleStream String) (C.CharityInfo)
readInfo (C.CharityID ident) = get 200 $ "/charity/" ++ ident ++ ".json"

main :: IO ()
main = do
    Log.start
    --run readInfoTest
    run registerTest

readInfoTest :: IO ()
readInfoTest = liftIO $ HTTP.browse $ do
    let user = J.UserLogin "greg" "greg"
    assertEqM "addUser"           (addUser user)                           (Right "greg")
    assertEqM "updateCharityInfo" (update ci)                              (Right ())
    assertEqM "getCharityInfo"    (readInfo (C.CharityID "gffw"))          (ci)

registerTest :: IO ()
registerTest = liftIO $ HTTP.browse $ do
    let user = J.UserLogin "greg" "greg"
    assertEqM "addUser"     (addUser user)                           (Right "greg")
    assertEqM "regiter"     (register mci)                           (Right ())

