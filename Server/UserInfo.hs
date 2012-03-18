module UserInfo where

import Data.Acid                             ( query, update )
import Data.List                             ( sortBy, group, sort )
import Data.Function                         ( on )
import Control.Monad.IO.Class                ( MonadIO, liftIO )
import Data.Login                            ( Identity )
import Data.CharityInfo                      ( CharityID )
import Data.UserInfo
import Query.DB
import ServerError

mostInfluential :: (MonadIO m) => Database -> m Identity
mostInfluential db = failLeftIO $ query db MostInfluentialUserQ

queryByOwner :: MonadIO m => Database -> Identity -> m UserInfo
queryByOwner db cid = failLeftIO $ query db (UserInfoByOwnerQ cid)

updateInfo :: MonadIO m  => Database -> Identity -> UserInfo -> m ()
updateInfo db uid ui
    | uid /= (owner ui) = badUsername
    | otherwise = liftIO $ update db (UserInfoU ui)

list :: Database -> IO ([Identity])
list db = query db (UsersQ)

popularCharities :: MonadIO m => Database -> m [CharityID]
popularCharities db = do 
    lst <- liftIO $ query db (UserCharitiesQ)
    let rv = map head $ reverse $ sortBy (compare `on` length) $ group $ sort lst
    liftIO $ print $ rv
    return $ rv

