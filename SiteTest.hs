module SiteTest where

import Site                                  ( site, Site(Site) )
import Control.Monad.IO.Class                ( MonadIO )
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

--interface
addUser :: A.UserLogin -> IO (Either SE.ServerError A.UserID)
addUser = post "/auth/add"

loginUser :: A.UserLogin -> IO (Either SE.ServerError A.UserID)
loginUser = post "/auth/login"

--returns the current user id if 
--they are logged in
checkUser :: IO (Either SE.ServerError A.UserID)
checkUser = get "/auth/check"

main :: IO ()
main = do
    let run tt = do tid <- forkIO emptyServer
                    _ <- tt
                    killThread tid
    run addUserTest
    run loginUserTest
    run checkUserTest

checkUserTest :: IO ()
checkUserTest = do
    --let user = (A.UserLogin (A.toB "anatoly") (A.toB "anatoly"))
    --    login = loginUser user
    --    add = addUser user
    assertEqM "check" checkUser (Left SE.CookieDecode)


loginUserTest :: IO ()
loginUserTest = do
    let 
        user = (A.UserLogin (A.toB "anatoly") (A.toB "anatoly"))
        login = loginUser user
        add = addUser user
    assertEqM "login" login (Left SE.UserDoesntExist)
    assertEqM "login" add (Right (A.toB "anatoly"))
    assertEqM "login" login (Right (A.toB "anatoly"))


addUserTest :: IO ()
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

post :: (JS.JSON b, JS.JSON a, MonadIO m) => String -> a -> m (Either SE.ServerError b) 
post url msg = do
    let 
            uri = fromMaybe (error $ "parse url: " ++ url) $ URI.parseURI (host ++ url)
            body = (JS.encode msg)
            req = HTTP.formToRequest $ HTTP.Form HTTP.POST uri [(body,"")]
    (_,hrsp) <- liftIO $ HTTP.browse $ HTTP.request $ req
    return $! checkJS $ JS.decode $ HTTP.rspBody hrsp
    where
       checkJS (JS.Ok a)     = a
       checkJS (JS.Error ss) = error ss

get :: (JS.JSON b, MonadIO m) => String -> m (Either SE.ServerError b) 
get url = do
    let req = HTTP.getRequest (host ++ url)
    (_,hrsp) <- liftIO $ HTTP.browse $ HTTP.request $ req
    return $! checkJS $ JS.decode $ HTTP.rspBody hrsp
    where
       checkJS (JS.Ok a)     = a
       checkJS (JS.Error ss) = error ss
