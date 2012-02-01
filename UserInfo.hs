{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fspec-constr-count=2 -fno-warn-orphans #-}
module UserInfo where

import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )
import Control.Monad.Error                   ( runErrorT, MonadError, throwError )
import Data.Data                             ( Typeable, Data )
import Data.Function                         ( on )
import Data.List                             ( sortBy )
import Control.Monad.IO.Class                ( MonadIO )
import qualified Data.Map                    as Map
import qualified Account                     as A

import Data.Acid
import Data.SafeCopy
import ServerError
import Data.UserInfo

data Database = Database !(Map.Map A.UserID UserInfo)
              deriving (Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Database)

empty :: Database
empty = Database Map.empty

updateU :: A.UserID -> UserInfo -> Update Database ()
updateU key val =  do
   (Database db) <- get
   put $ (Database $ Map.insert key val db)

lookupQ :: A.UserID -> Query Database (Either ServerError UserInfo)
lookupQ key = runErrorT $ do
   (Database db) <- ask
   checkMaybe UserDoesntExist $ Map.lookup key db

listQ :: Query Database ([A.UserID])
listQ = do
   (Database db) <- ask
   return $ Map.keys db

mostInfluentialQ :: Query Database (Either ServerError A.UserID)
mostInfluentialQ = runErrorT $ do
   (Database db) <- ask
   let 
        head' [] = throwError UserDoesntExist
        head' ls = return $ fst $ head ls
        influence aa = negate $ (centsDonated $ snd aa) + (alignedDonated $ snd aa)
   head' $ sortBy (compare `on` influence) $ Map.toList db

$(makeAcidic ''Database ['updateU, 'lookupQ, 'mostInfluentialQ, 'listQ])

mostInfluential :: (MonadIO m, MonadError ServerError m) => AcidState Database -> m A.UserID
mostInfluential db = A.rethrow $ query db MostInfluentialQ

lookupInfo :: (MonadIO m, MonadError ServerError m) => AcidState Database -> A.UserID -> m UserInfo
lookupInfo db cid = A.rethrow $ query db (LookupQ cid)

updateInfo :: AcidState Database -> A.UserID -> UserInfo -> IO ()
updateInfo db cid str = update db (UpdateU cid str)

list :: AcidState Database -> IO ([A.UserID])
list db = query db (ListQ)

