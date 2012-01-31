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
import qualified Data.ByteString.Lazy        as BL

import TestUtil
import ServerError

port :: Int
port = 8888

host :: String
host = "http://localhost:" ++ (show port)

--interface
addUser :: A.UserLogin -> IO (Either ServerError BL.ByteString)
addUser = post "/auth/add"

main :: IO ()
main = do
    addUserTest

addUserTest :: IO ()
addUserTest = do
    tid <- forkIO emptyServer
    let run = addUser (A.UserLogin (A.toB "anatoly") (A.toB "anatoly"))
    assertEqM "adduser" run (Right (A.toB "anatoly"))
    assertEqM "adduser" run (Left UserAlreadyExists)
    killThread tid

emptyServer :: IO ()
emptyServer = do
    ua <- openMemoryState A.empty
    ci <- openMemoryState C.empty
    ui <- openMemoryState U.empty
    let cfg = Happs.defaultServerConfig { Happs.port = port }
    Happs.serve (Just cfg) (site (Site ua ci ui))

post :: (JS.JSON b, JS.JSON a, MonadIO m) => String -> a -> m (Either ServerError b) 
post url msg = do
    let 
            uri = fromMaybe (error $ "parse url: " ++ url) $ URI.parseURI (host ++ url)
            body = (JS.encode msg)
            req = HTTP.formToRequest $ HTTP.Form HTTP.POST uri [(body,"")]
    (_,hrsp) <- liftIO $ HTTP.browse $ HTTP.request $ req
    return $! checkJS $ JS.decode $ HTTP.rspBody hrsp
    where
       checkJS (JS.Ok a)     = assertVal a
       checkJS (JS.Error ss) = error ss
       assertVal aa
           | (length (JS.encode aa)) < 0 = error "forcing evalation"
           | otherwise = aa
