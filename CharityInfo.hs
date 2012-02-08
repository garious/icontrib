{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fspec-constr-count=2 -fno-warn-orphans #-}
module CharityInfo where

import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )
import Control.Monad.Error                   ( runErrorT, MonadError )
import Control.Monad.IO.Class                ( MonadIO )
import qualified Data.Map                    as Map
import qualified Account                     as A
import Data.Acid
import ServerError
import Data.CharityInfo

empty :: Database
empty = Database Map.empty

updateU :: A.UserID -> CharityInfo -> Update Database ()
updateU key val =  do
   (Database db) <- get
   put $ (Database $ Map.insert key val db)

lookupQ :: A.UserID -> Query Database (Either ServerError CharityInfo)
lookupQ key = runErrorT $ do
   (Database db) <- ask
   checkMaybe UserDoesntExist $ Map.lookup key db

$(makeAcidic ''Database ['updateU, 'lookupQ])

lookupInfo :: (MonadIO m, MonadError ServerError m) => AcidState Database -> A.UserID -> m CharityInfo
lookupInfo db cid = A.rethrow $ query db (LookupQ cid)

updateInfo :: AcidState Database -> A.UserID -> CharityInfo -> IO ()
updateInfo db cid str = update db (UpdateU cid str)
