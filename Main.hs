import Site                                  ( site, Site(Site) )
import Happstack.Lite                        ( serve )
import Data.Acid.Memory                      ( openMemoryState )
import Control.Concurrent                    ( forkIO, killThread )
import Control.Monad.Error                   ( runErrorT )
import Char                                  ( ord )
import qualified Account                     as A
import qualified Data.ByteString.Lazy        as B
import qualified CharityInfo                 as C
import qualified UserInfo                    as U
import qualified Text.JSON                   as JS

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
    ui <- openMemoryState U.empty

    gregf <- readFile "public/donor/greg.json"
    tomf <- readFile "public/donor/tom.json"
    -- Hardcoded users
    _ <- runErrorT $ do
        A.addUser ua (toB "greg") (toB "greg")
        A.addUser ua (toB "anatoly") (toB "anatoly")
        A.addUser ua (toB "tom") (toB "tom")
    let 
        checkResult (JS.Ok a)    = return a
        checkResult (JS.Error ss) = error ss
    gi <- checkResult(JS.decode gregf)
    ti <- checkResult(JS.decode tomf)
    U.updateInfo ui (toB "greg") gi
    U.updateInfo ui (toB "tom") ti
    serve Nothing (site (Site ua ci ui))

-- String to ByteString
toB :: String -> B.ByteString
toB = B.pack . map (fromIntegral . ord)

