{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies, FlexibleContexts #-}
module Query.UserInfo where

import Control.Monad.State                   ( get, put, MonadState )
import Control.Monad.Reader                  ( ask, MonadReader )
import Data.Function                         ( on )
import Data.List                             ( sortBy, groupBy )
import Data.Generics                         ( listify )

import Data.CharityInfo                      ( CharityID(..) )
import Data.Login                            ( Identity )
import Data.Distribution                     ( Distribution, shares, cid, labels )
import JSONUtil                              ( jsonUpdate )
import Data.IxSet                            ( (@*) )
import qualified Data.IxSet                  as IxSet

import Data.Acid
import SiteError
import Data.UserInfo
import Data.DB

replace :: MonadState DB m => (UserInfoDB -> m UserInfoDB) -> m ()
replace ff = do
    db <- get
    ll <- ff (userInfos db)
    put $ db { userInfos = ll }

use :: MonadReader DB m => (UserInfoDB -> m b) -> m b
use ff = do
    db <- ask
    ff (userInfos db)

userInfoU :: UserInfo -> Update DB ()
userInfoU ui = replace $ \ db -> return (IxSet.updateIx (owner ui) ui db)

userInfoMergeU :: Identity -> String -> Update DB (Either String ())
userInfoMergeU uid body = runErrorT $ replace $ \ db -> do
    ui <- ((IxSet.getOne $ db @* [uid]) `justOr` (return empty))
    res <- jsonUpdate ui body
    return (IxSet.updateIx (owner res) res db)

userDistributionMQ :: (MonadError String m, MonadState DB m) => Identity -> m [Distribution]
userDistributionMQ uid = do
    db <- getU
    let sort' = sortBy (compare `on` (negate . shares))
    ui <- ((IxSet.getOne $ db @* [uid]) `justOr` doesntExist)
    return $ sort' $ distribution ui

userInfoByOwnerQ :: Identity -> Query DB (Either String UserInfo)
userInfoByOwnerQ key = runErrorT $ use $ \ db -> ((IxSet.getOne $ db @* [key]) `justOr` doesntExist)

usersQ :: Query DB ([Identity])
usersQ = use $ \ db -> return $ map owner $ IxSet.toList db

mostInfluentialUserQ:: Query DB (Either String Identity)
mostInfluentialUserQ= runErrorT $ use $ \ db -> do 
   let 
        head' [] = doesntExist
        head' ls = return $ owner $ head ls
        influence aa = negate $ (centsDonated aa) + (alignedDonated aa)
   head' $ sortBy (compare `on` influence) $ IxSet.toList db

userCharitiesQ :: Query DB ([CharityID])
userCharitiesQ = use $ \ db -> do 
    let isCharityID (CharityID _) = True
    return $ (listify isCharityID (IxSet.toList db))

programDistributionQ :: Query DB [Distribution]
programDistributionQ = use $ \ db -> do
   let 
        dists :: [Distribution]
        dists = concatMap distribution $ IxSet.toList db
        byCid :: [Distribution] -> [[Distribution]]
        byCid = groupBy ((==) `on` cid) . sortBy (compare `on` cid) 
        merge dd bb = dd { shares = (shares dd) + (shares bb), labels = [] }
   return $  map (foldl1 merge) $ byCid $ dists


