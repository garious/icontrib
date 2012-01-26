import Site                                  ( site, Site(Site) )
import Happstack.Lite                        ( serve )
import Data.Acid.Memory                      ( openMemoryState )
import Control.Concurrent                    ( forkIO, killThread )
import Control.Monad.Error                   ( runErrorT )
import Char                                  ( ord )
import qualified Account                     as A
import qualified Data.ByteString.Lazy        as B
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
    ci <- openMemoryState C.empty

    -- Hardcoded users
    _ <- runErrorT $ do
        A.addUser ua (toB "greg") (toB "greg")
        A.addUser ua (toB "anatoly") (toB "anatoly")

    serve Nothing (site (Site ua ci))

-- String to ByteString
toB :: String -> B.ByteString
toB = B.pack . map (fromIntegral . ord)

