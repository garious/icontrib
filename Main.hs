import Site                                  ( site, Site(Site), serve )
import Data.Acid.Memory                      ( openMemoryState )
import Control.Concurrent                    ( forkIO, killThread )
import Control.Monad                         ( forM )
import Control.Monad.Error                   ( runErrorT, liftIO )
import JSONUtil                              ( jsonDecode )
import qualified Data.ByteString.Lazy.Char8  as BS
import qualified Account                     as A
import qualified CharityInfo                 as C
import qualified UserInfo                    as U
import qualified Log                         as Log
import Happstack.Server                      ( TLSConf(TLSConf) )
import System.Path.Glob                      ( glob )
import System.FilePath                       ( takeBaseName )

main :: IO ()
main = do
    Log.start
    let loop = (webThread `catch` (\ _ -> loop))
    tid <- forkIO loop
    _ <- getLine
    killThread tid

webThread :: IO ()
webThread = do
    ua <- openMemoryState A.empty
    ci <- openMemoryState C.empty
    ui <- openMemoryState U.empty
    donors <- glob "private/donor/*.json"
    errs <- forM donors $ \ dd -> runErrorT $ do
        let name = BS.pack $ takeBaseName dd
        A.addUser ua (name) (name)
        ff <- liftIO $ readFile dd
        di <- jsonDecode ff
        liftIO $ U.updateInfo ui name di
    print errs
    Log.debugM  "Web server running. Press <enter> to exit."
    serve (Just ("localhost", TLSConf 8443 "testcert/server.crt" "testcert/server.key")) 8000 (site (Site ua ci ui))
    --serve Nothing 8000 (site (Site ua ci ui))

