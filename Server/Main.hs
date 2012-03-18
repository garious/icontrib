import Site                                  ( site, Site(Site), serve, redirectToSSL )
import Data.Acid                             ( openLocalStateFrom )
import Control.Concurrent                    ( forkIO, killThread )
import qualified Login                       as L
import qualified CharityInfo                 as C
import qualified UserInfo                    as U
import qualified Log                         as Log
import Happstack.Server.SimpleHTTPS          ( nullTLSConf, tlsPort, tlsCert, tlsKey )

main :: IO ()
main = do
    Log.start
    db <- database
    runMain False db

database :: IO Site
database = do
    ua <- openLocalStateFrom "private/db/accounts" A.empty
    ci <- openLocalStateFrom "private/db/charities" C.empty
    ui <- openLocalStateFrom "private/db/donors"U.empty
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
