{-# LANGUAGE OverloadedStrings #-}
module Site.DonorTest where

import Site.Browser
import qualified Log                         as Log
import Control.Monad.Trans                   ( liftIO )
import qualified Network.HTTP                as HTTP
import qualified Network.Browser             as HTTP
import qualified JSON.UserLogin              as J
import qualified Data.UserInfo               as U
import qualified Data.Login                  as L
import TestUtil


--interfaces over the browser (HTTP.BrowserAction (HTTP.HandleStream String)) monad
--adds the user and returns the user id, and sets the cookie value
addUser :: J.UserLogin ->  HTTP.BrowserAction (HTTP.HandleStream String) (Either String J.UserIdentity)
addUser = post 200 "/auth/add"

addUserInfo :: U.UserInfo ->  HTTP.BrowserAction (HTTP.HandleStream String) (Either String ())
addUserInfo = post 200 "/donor/update"

readUserInfo :: J.UserIdentity ->  HTTP.BrowserAction (HTTP.HandleStream String) (U.UserInfo)
readUserInfo ident = get 200 $ "/donor/" ++ ident ++ ".json"

mostInfluential :: HTTP.BrowserAction (HTTP.HandleStream String) (J.UserIdentity)
mostInfluential = get 200 "/donor/mostInfluential.json"

main :: IO ()
main = do
    Log.start
    run getInfoTest
    run mostInfluentialTest

getInfoTest :: IO ()
getInfoTest = liftIO $ HTTP.browse $ do
    let user = J.UserLogin "anatoly" "anatoly"
        toly = (L.Identity "anatoly")
        ui = U.UserInfo toly "first" "last" "phone" "email" "imageurl" 100 100 [] [] []
    --added new user, which should log us in
    assertEqM "addUser"      (addUser user)             (Right "anatoly")
    assertEqM "addUserInfo"  (addUserInfo ui)           (Right ())
    assertEqM "readUserInfo" (readUserInfo "anatoly")    (ui)

mostInfluentialTest :: IO ()
mostInfluentialTest = liftIO $ HTTP.browse $ do
    let user = J.UserLogin "anatoly" "anatoly"
        toly = (L.Identity "anatoly")
        ui = U.UserInfo toly "first" "last" "phone" "email" "imageurl" 100 100 [] [] []
    --added new user, which should log us in
    assertEqM "addUser"     (addUser user)      (Right "anatoly")
    assertEqM "addUserInfo" (addUserInfo ui)    (Right ())
    assertEqM "mostInfluential" mostInfluential ("anatoly")
