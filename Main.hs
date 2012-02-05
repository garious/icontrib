import Site                                  ( site, Site(Site), serve )
import Data.Acid.Memory                      ( openMemoryState )
import Control.Concurrent                    ( forkIO, killThread )
import Control.Monad.Error                   ( runErrorT, liftIO )
import JSONUtil                              ( jsonDecode )
import qualified Data.ByteString.Lazy.Char8  as BS
import qualified Account                     as A
import qualified CharityInfo                 as C
import qualified UserInfo                    as U
import qualified Log                         as Log
--import Happstack.Server                      ( TLSConf(TLSConf) )

main :: IO ()
main = do
    Log.start
    let loop = (webThread `catch` (\ _ -> loop))
    tid <- forkIO loop
    Log.debugM  "Web server running. Press <enter> to exit."
    _ <- getLine
    killThread tid

webThread :: IO ()
webThread = do
    ua <- openMemoryState A.empty
    ci <- openMemoryState C.empty
    ui <- openMemoryState U.empty

    tomf <- readFile "public/donor/tom.json"
    anon <- readFile "public/donor/anonymous.json"
    -- Hardcoded users
    _ <- runErrorT $ do
        ti <- jsonDecode tomf
        ai <- jsonDecode anon
        A.addUser ua (BS.pack "greg")     (BS.pack "greg")
        A.addUser ua (BS.pack "anatoly")  (BS.pack "anatoly")
        A.addUser ua (BS.pack "tom")      (BS.pack "tom")
        liftIO $ U.updateInfo ui (BS.pack "tom")       ti
        liftIO $ U.updateInfo ui (BS.pack "anonymous") ai
    --serve (Just ("localhost", TLSConf 8443 "testcert/server.crt" "testcert/server.key")) 8000 (site (Site ua ci ui))
    serve Nothing 8000 (site (Site ua ci ui))

