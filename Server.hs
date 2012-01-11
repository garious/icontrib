import Site                                  ( site, Site(Site) )
import Happstack.Lite                        ( serve )
import Data.Acid.Memory                      ( openMemoryState )
import Control.Concurrent                    ( forkIO, killThread )
import qualified Account                     as A
import qualified CharityInfo                 as C

main :: IO ()
main = do
    tid <- forkIO webThread
    putStrLn "Web server running. Press <enter> to exit."
    _ <- getLine
    killThread tid

webThread :: IO ()
webThread = do
    ua <- openMemoryState A.empty
    ca <- openMemoryState A.empty
    ci <- openMemoryState C.empty
    serve Nothing (site (Site ua ca ci))


