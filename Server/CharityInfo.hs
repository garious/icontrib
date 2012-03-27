{-# LANGUAGE FlexibleContexts #-}
module CharityInfo where

import Data.CharityInfo
import Data.Login                            ( Identity )
import Data.Acid                             ( query, update )
import SiteError
import Data.Popular                          ( Popular(Popular) )
import Query.DB

queryByOwner :: MonadIO m => Database -> Identity -> m [CharityInfo]
queryByOwner db uid = liftIO $ query db (CharityByOwnerQ uid)

updateInfo :: (MonadError String m, MonadIO m) => Database -> Identity -> CharityInfo -> m () 
updateInfo db uid ci
    | uid /= (owner ci) = badUsername
    | otherwise = throwLeft $ update db (CharityInfoU ci)

deleteByEin :: (MonadError String m, MonadIO m) => Database -> Identity -> Ein -> m () 
deleteByEin db uid cein = throwLeft $ update db (DeleteCharityByEinU uid cein)

toPopular :: MonadIO m => Database -> [CharityID] -> m ([Popular])
toPopular db cids = do
    infos <- liftIO $ query db (CharityByIDQ cids)
    return $ map (\ ci -> Popular (cid ci) (organizationName ci) (imageUrl ci)) infos

queryByCID :: (MonadError String m, MonadIO m) => Database -> CharityID -> m CharityInfo
queryByCID db cc = do
    infos <- liftIO $ query db (CharityByIDQ [cc])
    let singleton' [a] = return a
        singleton' _   = badCharityID
    singleton' infos 
