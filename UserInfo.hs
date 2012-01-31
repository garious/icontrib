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

data Fund = Fund { name :: String 
                 , url :: String
                 , shares :: Int
                 , labels :: [String]
                 }
          deriving (Show, Typeable, Data, Eq)

$(deriveSafeCopy 0 'base ''Fund)

data UserInfo = UserInfo { firstName        :: String
                         , lastName         :: String
                         , imageUrl         :: String
                         , centsDonated     :: Int
                         , alignedDonated   :: Int
                         , distribution     :: [Fund]
                         , funds :: [String]
                         }
              deriving (Show, Typeable, Data, Eq)

$(deriveSafeCopy 0 'base ''UserInfo)


data Database = Database !(Map.Map A.UserID UserInfo)
              deriving (Show, Typeable)
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


mostInfluentialQ :: Query Database (Either ServerError UserInfo)
mostInfluentialQ = runErrorT $ do
   (Database db) <- ask
   let 
        values mp = snd $ unzip $ Map.toList mp
        head' [] = throwError UserDoesntExist
        head' ls = return $ head ls
        influence aa = negate $ (centsDonated aa) + (alignedDonated aa)
   head' $ sortBy (compare `on` influence) $ values db 

$(makeAcidic ''Database ['updateU, 'lookupQ, 'mostInfluentialQ, 'listQ])

mostInfluential :: (MonadIO m, MonadError ServerError m) => AcidState Database -> m UserInfo
mostInfluential db = A.rethrow $ query db MostInfluentialQ

lookupInfo :: (MonadIO m, MonadError ServerError m) => AcidState Database -> A.UserID -> m UserInfo
lookupInfo db cid = A.rethrow $ query db (LookupQ cid)

updateInfo :: AcidState Database -> A.UserID -> UserInfo -> IO ()
updateInfo db cid str = update db (UpdateU cid str)

list :: AcidState Database -> IO ([A.UserID])
list db = query db (ListQ)

