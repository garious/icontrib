{-# LANGUAGE OverloadedStrings #-}

import Site                                  ( site, Site(Site), jsonDecode )
import Happstack.Lite                        ( serve )
import Data.Acid.Memory                      ( openMemoryState )
import Control.Concurrent                    ( forkIO, killThread )
import Control.Monad.Error                   ( runErrorT )
import qualified Account                     as A
import qualified CharityInfo                 as C
import qualified UserInfo                    as U

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

    tomf <- readFile "public/donor/tom.json"
    anon <- readFile "public/donor/anonymous.json"
    -- Hardcoded users
    _ <- runErrorT $ do
        A.addUser ua "greg" "greg"
        A.addUser ua "anatoly" "anatoly"
        A.addUser ua "tom" "tom"
    let 
        checkResult (Just a) = return a
        checkResult Nothing  = error "main: checkResult: reading default json files" 
    ti <- checkResult (jsonDecode tomf)
    ai <- checkResult (jsonDecode anon)
    U.updateInfo ui "tom" ti
    U.updateInfo ui "anonymous" ai
    serve Nothing (site (Site ua ci ui))

