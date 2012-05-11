{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies, FlexibleContexts #-}
module Query.Stats where

import Control.Monad.Reader                  ( ask, MonadReader )
import Data.Function                         ( on )
import Data.List                             ( sortBy, groupBy )

import Data.Distribution                     ( Distribution, shares, cid, labels )
import qualified Data.IxSet                  as IxSet

import Data.Acid
import Data.UserInfo
import Data.DB
import Data.Stats(Stats(Stats))


use :: MonadReader DB m => (UserInfoDB -> m b) -> m b
use ff = do
    db <- ask
    ff (userInfos db)


programDistributionQ :: Query DB Stats
programDistributionQ = use $ \ db -> do
   let 
        userDists :: UserInfo -> [Distribution]
        userDists ui = map (\ dd -> dd { shares = (shares dd) * (fromIntegral $ centsDonated ui) } ) (distribution ui)
        dists :: [Distribution]
        dists = concatMap userDists $ IxSet.toList db
        byCid :: [Distribution] -> [[Distribution]]
        byCid = groupBy ((==) `on` cid) . sortBy (compare `on` cid) 
        merge dd bb = dd { shares = (shares dd) + (shares bb), labels = [] }
        merged = map (foldl1 merge) $ byCid $ dists
        total = sum $ map shares merged
        
   return $ Stats total $ map (\ dd -> dd { shares = (shares dd) / total } ) merged
