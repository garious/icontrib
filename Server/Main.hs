import Site                                  ( site, serve, redirectToSSL )
import Control.Concurrent                    ( forkIO, killThread )
import qualified Log                         as Log
import qualified DB                          as DB
import Happstack.Server.SimpleHTTPS          ( nullTLSConf, tlsPort, tlsCert, tlsKey )

main :: IO ()
main = do
    Log.start
    db <- DB.newFromFile
    runMain False db

type WithSSL = Bool

runMain :: WithSSL -> DB.Database -> IO ()
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
