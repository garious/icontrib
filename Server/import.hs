import Data.Acid    ( createCheckpoint )
import qualified CharityInfo                 as C
import qualified Data.CharityInfo            as C
import qualified UserInfo                    as U
import qualified Data.UserInfo               as U
import System.Path.Glob                      ( glob )
import Control.Monad                         ( forM )
import Control.Monad.Error                   ( runErrorT, liftIO )
import JSONUtil                              ( jsonDecode )
import Control.Applicative                   ( (<|>) )
import System.Environment                    ( getArgs )
import Opts                                  ( getOptions, dbDir )

import qualified Login                       as L
import qualified Data.Login                  as L
import qualified DB                          as DB
main :: IO ()
main = do
    args <- getArgs
    opts <- getOptions args
    db <- DB.newFromFile (dbDir opts)
    let errorLeft (Left ee) = error ee
        errorLeft (Right _) = return ()
    donors <- glob "private/static/donor/*.json"
    e1 <- forM donors $ \ dd -> runErrorT $ do
        ff <- liftIO $ readFile dd
        liftIO $ print dd
        di <- jsonDecode ff
        liftIO $ print di
        let ident@(L.Identity name) = (U.owner di)
        (L.addIdentity db ident name) <|> return ()
        U.updateInfo db ident di
    mapM_ errorLeft e1
    print "done donors"
    charities <- glob "private/static/charity/*.json"
    e2 <- forM charities $ \ dd -> runErrorT $ do
        ff <- liftIO $ readFile dd
        liftIO $ print dd
        di <- jsonDecode ff
        liftIO $ print di
        C.updateInfo db (C.owner di) di
    mapM_ errorLeft e2
    print "done charities"
    createCheckpoint db
