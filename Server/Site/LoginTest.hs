module Site.LoginTest where

import Site.Browser
import qualified Log                         as Log
import Control.Monad.Trans                   ( liftIO )
import qualified Network.HTTP                as HTTP
import qualified Network.Browser             as HTTP
import qualified Data.JSON                   as J
import TestUtil


--interfaces over the browser (HTTP.BrowserAction (HTTP.HandleStream String)) monad
--adds the user and returns the user id, and sets the cookie value
addUser :: J.UserLogin ->  HTTP.BrowserAction (HTTP.HandleStream String) (Either String J.UserIdentity)
addUser = post "/auth/add"

--logs in the user, changing the cookie value
loginUser :: J.UserLogin -> HTTP.BrowserAction (HTTP.HandleStream String) (Either String J.UserIdentity)
loginUser = post "/auth/login"

--returns the current logged in user id based on the cookie
checkUser :: HTTP.BrowserAction (HTTP.HandleStream String) (Either String J.UserIdentity)
checkUser = get "/auth/check.json"

--logged out the current user
logoutUser :: HTTP.BrowserAction (HTTP.HandleStream String) (Either String ())
logoutUser = post "/auth/logout" ()

main :: IO ()
main = do
    Log.start
    run addUserTest
    run loginUserTest
    run checkUserTest
    run logoutUserTest

logoutUserTest :: IO ()
logoutUserTest = liftIO $ HTTP.browse $ do
    let user = J.UserLogin "anatoly" "anatoly"
        add = addUser user
    assertEqM "logout" checkUser    (Left "CookieDecodeError")
    assertEqM "logout" add          (Right "anatoly")
    assertEqM "logout" checkUser    (Right "anatoly")
    assertEqM "logout" logoutUser   (Right ())
    assertEqM "logout" checkUser    (Left "BadToken")

checkUserTest :: IO ()
checkUserTest = liftIO $ HTTP.browse $ do
    let user = J.UserLogin "anatoly" "anatoly"
        add = addUser user
    --empty server, no cookie in browser
    assertEqM "check" checkUser     (Left "CookieDecodeError")
    --added new user, which should log us in
    assertEqM "check" add           (Right "anatoly")
    --check if we are logged in
    assertEqM "check" checkUser     (Right "anatoly")

loginUserTest :: IO ()
loginUserTest = liftIO $ HTTP.browse $ do
    let 
        user = J.UserLogin "anatoly" "anatoly"
        login = loginUser user
        add = addUser user
    assertEqM "login" login (Left "DoesntExist")
    assertEqM "login" add   (Right "anatoly")
    assertEqM "login" login (Right "anatoly")

addUserTest ::  IO ()
addUserTest = liftIO $ HTTP.browse $ do
    let addu = addUser (J.UserLogin "anatoly" "anatoly")
    assertEqM "add" addu (Right "anatoly")
    assertEqM "add" addu (Left "AlreadyExists")


