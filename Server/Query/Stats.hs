{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies, FlexibleContexts #-}
module Query.Stats where

import Control.Monad.Reader                  ( ask, MonadReader )
import Data.Function                         ( on )
import Data.List                             ( sortBy, groupBy )

import Data.Distribution                     ( Distribution, shares, cid, labels )
import qualified Data.IxSet                  as IxSet
import qualified Data.Stats as S

import Data.Acid
import qualified Data.UserInfo as U
import Data.DB


use :: MonadReader DB m => (U.UserInfoDB -> m b) -> m b
use ff = do
    db <- ask
    ff (userInfos db)


programDistributionQ :: Query DB S.Stats
programDistributionQ = use $ \ db -> do
   let 
        userDists :: U.UserInfo -> [Distribution]
        userDists ui = map (\ dd -> dd { shares = (shares dd) * (fromIntegral $ U.centsDonated ui) } ) (U.distribution ui)
        dists :: [Distribution]
        dists = concatMap userDists $ IxSet.toList db
        byCid :: [Distribution] -> [[Distribution]]
        byCid = groupBy ((==) `on` cid) . sortBy (compare `on` cid) 
        merge dd bb = dd { shares = (shares dd) + (shares bb), labels = [] }
        merged = map (foldl1 merge) $ byCid $ dists
        total = sum $ map shares merged
   return ( S.empty { S.centsDonated = total 
                    , S.distribution = map (\ dd -> dd { shares = (shares dd) / total } ) merged
                    } )
