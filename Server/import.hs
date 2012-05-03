import Data.Acid    ( createCheckpoint )
import qualified DB.CharityInfo              as C
import qualified Data.CharityInfo            as C
import qualified DB.UserInfo                 as U
import qualified Data.UserInfo               as U
import System.Path.Glob                      ( glob )
import Control.Monad                         ( forM )
import Control.Monad.Error                   ( runErrorT, liftIO )
import JSONUtil                              ( jsonUpdate )
import Control.Applicative                   ( (<|>) )
import System.Environment                    ( getArgs )
import Opts                                  ( getOptions, dbDir )

import qualified DB.Login                    as L
import qualified Data.Login                  as L
import qualified DB.DB                       as DB
main :: IO ()
main = do
    args <- getArgs
    opts <- getOptions args
    putStrLn "importing..."
    putStrLn $ "if you see an error you might need to : rm -rf " ++ (dbDir opts)
    db <- DB.newFromFile (dbDir opts)
    let errorLeft (Left ee) = error ee
        errorLeft (Right _) = return ()
    donors <- glob "private/static/donor/*.json"
    e1 <- forM donors $ \ dd -> runErrorT $ do
        body <- liftIO $ readFile dd
        di <- jsonUpdate U.empty body
        let ident@(L.Identity name) = (U.owner di)
        (L.addIdentity db ident name) <|> return ()
        U.updateInfo db ident body
    mapM_ errorLeft e1
    putStrLn "done importing donors"
    charities <- glob "private/static/charity/*.json"
    e2 <- forM charities $ \ dd -> runErrorT $ do
        body <- liftIO $ readFile dd
        di <- jsonUpdate C.empty body
        C.updateInfo db (C.owner di) di
    mapM_ errorLeft e2
    putStrLn "done importing charities"
    createCheckpoint db
