module CharityInfo where

import Data.CharityInfo
import Data.Login                            ( Identity )
import Control.Monad.IO.Class                ( MonadIO, liftIO )
import Data.Acid                             ( query, update )
import ServerError
import Data.Popular                          ( Popular(Popular) )
import Query.DB

queryByOwner :: MonadIO m => Database -> Identity -> m [CharityInfo]
queryByOwner db uid = liftIO $ query db (CharityByOwnerQ uid)

updateInfo :: (MonadIO m) => Database -> Identity -> CharityInfo -> m ()
updateInfo db uid ci
    | uid /= (owner ci) = badUsername
    | otherwise = failLeftIO $ update db (CharityInfoU ci)

deleteByEin :: (MonadIO m) => Database -> Identity -> Ein -> m ()
deleteByEin db uid cein = failLeftIO $ update db (DeleteCharityByEinU uid cein)

toPopular :: MonadIO m => Database -> [CharityID] -> m ([Popular])
toPopular db cids = do
    infos <- liftIO $ query db (CharityByIDQ cids)
    return $ map (\ ci -> Popular (cid ci) (organizationName ci) ) infos

queryByCID :: (MonadIO m) => Database -> CharityID -> m CharityInfo
queryByCID db cc = do
    infos <- liftIO $ query db (CharityByIDQ [cc])
    let singleton' [a] = return a
        singleton' _   = fail "BadCharityID"
    singleton' infos 
