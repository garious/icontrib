import Data.Acid    ( openLocalStateFrom, createCheckpoint )
import qualified Account                     as A
import qualified CharityInfo                 as C
import qualified Data.CharityInfo            as C
import qualified UserInfo                    as U
import qualified Data.UserInfo               as U
import System.Path.Glob                      ( glob )
import Control.Monad                         ( forM )
import Control.Monad.Error                   ( runErrorT, liftIO )
import JSONUtil                              ( jsonDecode )
import Control.Applicative                   ( (<|>) )

main :: IO ()
main = do
    let errorLeft (Left ee) = error ee
        errorLeft (Right _) = return ()
    ua <- openLocalStateFrom "private/db/accounts" A.empty
    ci <- openLocalStateFrom "private/db/charities" C.empty
    ui <- openLocalStateFrom "private/db/donors" U.empty
    donors <- glob "private/static/donor/*.json"
    e1 <- forM donors $ \ dd -> runErrorT $ do
        ff <- liftIO $ readFile dd
        liftIO $ print dd
        di <- jsonDecode ff
        liftIO $ print di
        (A.addUser ua (U.owner di) (U.owner di)) <|> return ()
        U.updateInfo ui (U.owner di) di
    mapM_ errorLeft e1
    print "done donors"
    charities <- glob "private/static/charity/*.json"
    e2 <- forM charities $ \ dd -> runErrorT $ do
        ff <- liftIO $ readFile dd
        liftIO $ print dd
        di <- jsonDecode ff
        liftIO $ print di
        C.updateInfo ci (C.owner di) di
    mapM_ errorLeft e2
    print "done charities"
    createCheckpoint ua
    createCheckpoint ci
    createCheckpoint ui
