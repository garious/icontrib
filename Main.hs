import Site                                  ( site, Site(Site) )
import Happstack.Lite                        ( serve )
import Data.Acid.Memory                      ( openMemoryState )
import Control.Concurrent                    ( forkIO, killThread )
import Control.Monad.Error                   ( runErrorT )
import qualified Account                     as A
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
        A.addUser ua (A.toB "greg") (A.toB "greg")
        A.addUser ua (A.toB "anatoly") (A.toB "anatoly")
        A.addUser ua (A.toB "tom") (A.toB "tom")
    let 
        checkResult (JS.Ok a)    = return a
        checkResult (JS.Error ss) = error ss
    gi <- checkResult(JS.decode gregf)
    ti <- checkResult(JS.decode tomf)
    U.updateInfo ui (A.toB "greg") gi
    U.updateInfo ui (A.toB "tom") ti
    serve Nothing (site (Site ua ci ui))

