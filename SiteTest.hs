{-# LANGUAGE OverloadedStrings #-}

module SiteTest where

import Site                                  ( site, Site(Site), serve )
import Data.Acid.Memory                      ( openMemoryState )
import Control.Concurrent                    ( forkIO, killThread )
import Control.Monad.Trans                   ( liftIO )
import Data.Maybe                            ( fromMaybe )
import qualified Account                     as A
import qualified CharityInfo                 as C
import qualified UserInfo                    as U
import Text.JSON.Generic                     as JS
import qualified Network.HTTP                as HTTP
import qualified Network.Browser             as HTTP
import qualified Network.URI                 as URI
import qualified ServerError                 as SE
import qualified JSONUtil                    as JS
import TestUtil

port :: Int
port = 8888

host :: String
host = "http://localhost:" ++ (show port)

--interfaces over the browser (HTTP.BrowserAction (HTTP.HandleStream String)) monad
--adds the user and returns the user id, and sets the cookie value
addUser :: A.UserLogin ->  HTTP.BrowserAction (HTTP.HandleStream String) (Either SE.ServerError A.UserID)
addUser = post "/auth/add"

--logs in the user, changing the cookie value
loginUser :: A.UserLogin -> HTTP.BrowserAction (HTTP.HandleStream String) (Either SE.ServerError A.UserID)
loginUser = post "/auth/login"

--returns the current logged in user id based on the cookie
checkUser :: HTTP.BrowserAction (HTTP.HandleStream String) (Either SE.ServerError A.UserID)
checkUser = get "/auth/check"

--logged out the current user
logoutUser :: HTTP.BrowserAction (HTTP.HandleStream String) (Either SE.ServerError ())
logoutUser = post "/auth/logout" ()

main :: IO ()
main = do
    let run tt = do tid <- forkIO emptyServer
                    _ <- tt
                    killThread tid
    run addUserTest
    run loginUserTest
    run checkUserTest
    run logoutUserTest

logoutUserTest :: IO ()
logoutUserTest = liftIO $ HTTP.browse $ do
    let user = A.UserLogin "anatoly" "anatoly"
        add = addUser user
    assertEqM "logout" checkUser (Left "CookieDecodeError")
    assertEqM "logout" add (Right "anatoly")
    assertEqM "logout" checkUser (Right "anatoly")
    assertEqM "logout" logoutUser (Right ())
    assertEqM "logout" checkUser (Left "BadCookie")

checkUserTest :: IO ()
checkUserTest = liftIO $ HTTP.browse $ do
    let user = A.UserLogin "anatoly" "anatoly"
        add = addUser user
    --empty server, no cookie in browser
    assertEqM "check" checkUser (Left "CookieDecodeError")
    --added new user, which should log us in
    assertEqM "check" add (Right "anatoly")
    --check if we are logged in
    assertEqM "check" checkUser (Right "anatoly")

loginUserTest :: IO ()
loginUserTest = liftIO $ HTTP.browse $ do
    let 
        user = A.UserLogin "anatoly" "anatoly"
        login = loginUser user
        add = addUser user
    assertEqM "login" login (Left "DoesntExist")
    assertEqM "login" add (Right "anatoly")
    assertEqM "login" login (Right "anatoly")

addUserTest ::  IO ()
addUserTest = liftIO $ HTTP.browse $ do
    let run = addUser (A.UserLogin "anatoly" "anatoly")
    assertEqM "add" run (Right "anatoly")
    assertEqM "add" run (Left "AlreadyExists")

emptyServer :: IO ()
emptyServer = do
    ua <- openMemoryState A.empty
    ci <- openMemoryState C.empty
    ui <- openMemoryState U.empty
    serve (Right port) (site (Site ua ci ui))

post :: (Data b, Data a) => String -> a -> HTTP.BrowserAction (HTTP.HandleStream String) (Either SE.ServerError b) 
post url msg = do
    let 
            uri = fromMaybe (error $ "parse url: " ++ url) $ URI.parseURI (host ++ url)
            body = (JS.jsonEncode msg)
            req = HTTP.formToRequest $ HTTP.Form HTTP.POST uri [(body,"")]
    (_,hrsp) <- HTTP.request $ req
    return $! JS.jsonDecodeE $ HTTP.rspBody hrsp

get :: (Data b) => String ->  HTTP.BrowserAction (HTTP.HandleStream String) (Either SE.ServerError b) 
get url = do
    let req = HTTP.getRequest (host ++ url)
    (_,hrsp) <- HTTP.request $ req
    return $! JS.jsonDecodeE $ HTTP.rspBody hrsp
