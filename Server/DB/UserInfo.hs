{-# LANGUAGE FlexibleContexts #-}
module DB.UserInfo where

import Data.Acid                             ( query, update )
import Data.List                             ( sortBy, group, sort )
import Data.Function                         ( on )
import Data.Login                            ( Identity )
import Data.CharityInfo                      ( CharityID )
import Data.Stats                            ( Stats )
import Data.UserInfo
import Query.DB
import SiteError

mostInfluential :: (MonadError String m, MonadIO m) => Database -> m Identity
mostInfluential db = throwLeft $ query db MostInfluentialUserQ

programDistribution :: (MonadIO m) => Database -> m Stats
programDistribution db = liftIO $ query db ProgramDistributionQ

queryByOwner :: (MonadError String m, MonadIO m) => Database -> Identity -> m UserInfo
queryByOwner db cid = throwLeft $ query db (UserInfoByOwnerQ cid)

updateInfo :: (MonadError String m, MonadIO m)  => Database -> Identity -> String -> m ()
updateInfo db uid ui = throwLeft $ update db (UserInfoMergeU uid ui)

list :: MonadIO m => Database -> m ([Identity])
list db = liftIO $ query db (UsersQ)

popularCharities :: MonadIO m => Database -> m [CharityID]
popularCharities db = do 
    lst <- liftIO $ query db (UserCharitiesQ)
    let rv = map head $ reverse $ sortBy (compare `on` length) $ group $ sort lst
    liftIO $ print $ rv
    return $ rv

