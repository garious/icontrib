import Account                               ( empty )
import Site                                  ( site )
import Happstack.Lite                        ( serve )
import Data.Acid.Memory                      ( openMemoryState )
import Control.Concurrent                    ( forkIO, killThread )

main :: IO ()
main = do
    tid <- forkIO webThread
    putStrLn "Web server running. Press <enter> to exit."
    _ <- getLine
    killThread tid

webThread :: IO ()
webThread = do
    db <- openMemoryState empty
    serve Nothing (site db)


