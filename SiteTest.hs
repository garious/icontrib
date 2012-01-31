module SiteTest where

import Site                                  ( site, Site(Site) )
import Data.Acid.Memory                      ( openMemoryState )
import Control.Concurrent                    ( forkIO, killThread )
import Control.Monad.Trans                   ( liftIO )
import Data.Maybe                            ( fromMaybe )
import qualified Account                     as A
import qualified CharityInfo                 as C
import qualified UserInfo                    as U
import qualified Text.JSON                   as JS
import qualified Network.HTTP                as HTTP
import qualified Network.Browser             as HTTP
import qualified Network.URI                 as URI
import qualified Happstack.Lite              as Happs
import qualified ServerError                 as SE
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

main :: IO ()
main = do
    let run tt = do tid <- forkIO emptyServer
                    _ <- liftIO $ HTTP.browse $ tt
                    killThread tid
    run addUserTest
    run loginUserTest
    run checkUserTest

checkUserTest :: HTTP.BrowserAction (HTTP.HandleStream String) ()
checkUserTest = do
    let user = (A.UserLogin (A.toB "anatoly") (A.toB "anatoly"))
        add = addUser user
    assertEqM "check" checkUser (Left SE.CookieDecode)
    assertEqM "check" add (Right (A.toB "anatoly"))
    assertEqM "check" checkUser (Right (A.toB "anatoly"))

loginUserTest :: HTTP.BrowserAction (HTTP.HandleStream String) ()
loginUserTest = do
    let 
        user = (A.UserLogin (A.toB "anatoly") (A.toB "anatoly"))
        login = loginUser user
        add = addUser user
    assertEqM "login" login (Left SE.UserDoesntExist)
    assertEqM "login" add (Right (A.toB "anatoly"))
    assertEqM "login" login (Right (A.toB "anatoly"))

addUserTest ::  HTTP.BrowserAction (HTTP.HandleStream String) ()
addUserTest = do
    let run = addUser (A.UserLogin (A.toB "anatoly") (A.toB "anatoly"))
    assertEqM "add" run (Right (A.toB "anatoly"))
    assertEqM "add" run (Left SE.UserAlreadyExists)

emptyServer :: IO ()
emptyServer = do
    ua <- openMemoryState A.empty
    ci <- openMemoryState C.empty
    ui <- openMemoryState U.empty
    let cfg = Happs.defaultServerConfig { Happs.port = port }
    Happs.serve (Just cfg) (site (Site ua ci ui))

post :: (JS.JSON b, JS.JSON a) => String -> a -> HTTP.BrowserAction (HTTP.HandleStream String) (Either SE.ServerError b) 
post url msg = do
    let 
            uri = fromMaybe (error $ "parse url: " ++ url) $ URI.parseURI (host ++ url)
            body = (JS.encode msg)
            req = HTTP.formToRequest $ HTTP.Form HTTP.POST uri [(body,"")]
    (_,hrsp) <- HTTP.request $ req
    return $! checkJS $ JS.decode $ HTTP.rspBody hrsp
    where
       checkJS (JS.Ok a)     = a
       checkJS (JS.Error ss) = error ss

get :: (JS.JSON b) => String ->  HTTP.BrowserAction (HTTP.HandleStream String) (Either SE.ServerError b) 
get url = do
    let req = HTTP.getRequest (host ++ url)
    (_,hrsp) <- HTTP.request $ req
    return $! checkJS $ JS.decode $ HTTP.rspBody hrsp
    where
       checkJS (JS.Ok a)     = a
       checkJS (JS.Error ss) = error ss
