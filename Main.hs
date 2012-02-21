import Site                                  ( site, Site(Site), serve, redirectToSSL )
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
import Happstack.Server.SimpleHTTPS          ( nullTLSConf, tlsPort, tlsCert, tlsKey )
import System.Path.Glob                      ( glob )
import System.FilePath                       ( takeBaseName )

main :: IO ()
main = do
    Log.start
    db <- database
    runMain True db

database :: IO Site
database = do
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
    Log.debugM (show errs)
    return (Site ua ci ui)

type WithSSL = Bool

runMain :: WithSSL -> Site -> IO ()
runMain False db = do
    tid <- forkIO $ do 
        Log.debugM  "Web server running. Press <enter> to exit."
        (serve (Right 8000) (site db))
    _ <- getLine
    killThread tid

runMain True db = do
    let tlsconf = nullTLSConf { tlsPort = 8443, tlsCert = "testcert/server.crt", tlsKey = "testcert/server.key"}
    rtid <- forkIO (redirectToSSL tlsconf "localhost" 8000)
    tid <- forkIO $ do
        Log.debugM  "Web server running. Press <enter> to exit."
        serve (Left tlsconf)  (site db)
    _ <- getLine
    killThread rtid
    killThread tid
