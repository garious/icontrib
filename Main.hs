import Site                                  ( site, Site(Site), serve, redirectToSSL )
import Data.Acid    ( openLocalStateFrom )
import Control.Concurrent                    ( forkIO, killThread )
import qualified Account                     as A
import qualified CharityInfo                 as C
import qualified UserInfo                    as U
import qualified Log                         as Log
import Happstack.Server.SimpleHTTPS          ( nullTLSConf, tlsPort, tlsCert, tlsKey )
import Opts                                  ( getOptions, dbDir, httpPort )
import System.Environment                    ( getArgs )
import System.FilePath                       ( (</>) )

main :: IO ()
main = do
    args <- getArgs
    opts <- getOptions args
    Log.start
    db <- database (dbDir opts)
    runMain False (httpPort opts) db

database :: FilePath -> IO Site
database fp = do
    ua <- openLocalStateFrom (fp </> "accounts") A.empty
    ci <- openLocalStateFrom (fp </> "charities") C.empty
    ui <- openLocalStateFrom (fp </> "donors") U.empty
    return (Site ua ci ui)

type WithSSL = Bool

runMain :: WithSSL -> Int -> Site -> IO ()
runMain False port db = do
    tid <- forkIO $ do 
        Log.debugM  "Web server running. Press <enter> to exit."
        (serve (Right port) (site db))
    _ <- getLine
    killThread tid

runMain True port db = do
    let tlsconf = nullTLSConf { tlsPort = 8443, tlsCert = "testcert/server.crt", tlsKey = "testcert/server.key"}
    rtid <- forkIO (redirectToSSL tlsconf "localhost" port)
    tid <- forkIO $ do
        Log.debugM  "Web server running. Press <enter> to exit."
        serve (Left tlsconf)  (site db)
    _ <- getLine
    killThread rtid
    killThread tid
