{-# LANGUAGE OverloadedStrings #-}
module Site.Browser where
import qualified Log                         as Log
import Control.Monad                         ( when )
import Site                                  ( site, serve )
import Control.Concurrent                    ( forkIO, killThread, threadDelay )
import Data.Maybe                            ( fromMaybe )
 
import qualified DB.DB                       as DB
import Text.JSON.Generic                     as JS
import qualified Network.HTTP                as HTTP
import qualified Network.Browser             as HTTP
import qualified Network.URI                 as URI
import qualified JSONUtil                    as JS


port :: Int
port = 8888

host :: String
host = "http://localhost:" ++ (show port)

run :: IO t -> IO ()
run tt = do 
    tid <- forkIO (emptyServer)
    threadDelay 2000
    _ <- tt
    killThread tid

post :: (Data b, Data a) => Int -> String -> a -> HTTP.BrowserAction (HTTP.HandleStream String) (b) 
post code url msg = do
    let 
            uri = fromMaybe (error $ "parse url: " ++ url) $ URI.parseURI (host ++ url)
            body = (JS.jsonEncode msg)
            req = HTTP.formToRequest $ HTTP.Form HTTP.POST uri [(body,"")]
    (_,hrsp) <- HTTP.request $ req
    when ((toCode $ HTTP.rspCode hrsp) /= code) $ error "site.browser.post: unexpected response code" 
    return $! JS.jsonDecodeE $ HTTP.rspBody hrsp

get :: (Data b) => Int -> String ->  HTTP.BrowserAction (HTTP.HandleStream String) (b) 
get code url = do
    let req = HTTP.getRequest (host ++ url)
    (_,hrsp) <- HTTP.request $ req
    when ((toCode $ HTTP.rspCode hrsp) /= code) $ error "site.browser.post: unexpected response code" 
    return $! JS.jsonDecodeE $ HTTP.rspBody hrsp

emptyServer :: IO ()
emptyServer = do
    Log.debugM  "server running"
    db <- DB.emptyMemoryDB
    serve (Right port) (site db)

toCode :: Num a => (a, a, a) -> a
toCode (x,y,z) = x* 100 + y * 10 + z
