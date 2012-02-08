{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fspec-constr-count=2 -fno-warn-orphans #-}
module CharityInfo where

import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )
import Control.Monad.Error                   ( runErrorT, MonadError )
import Control.Monad                         ( liftM )
import Control.Monad.IO.Class                ( MonadIO )
import Data.Maybe                            ( fromMaybe )
import qualified Data.Map                    as Map
import qualified Account                     as A
import Data.Acid
import ServerError
import Data.CharityInfo

empty :: Database
empty = Database Map.empty

deleteU :: A.UserID -> Ein -> Update Database ()
deleteU user cien = do
    (Database db) <- get
    let def = Map.empty
        nud = fromMaybe def $ do
            ud <- Map.lookup user db
            return $ Map.delete cien ud
    put $ (Database $ Map.insert user nud db)

updateU :: A.UserID -> CharityInfo -> Update Database ()
updateU user ci =  do
    (Database db) <- get
    let def = Map.fromList [((ein (info ci)), ci)]
        nud = fromMaybe def $ do
            ud <- Map.lookup user db
            return $ Map.insert (ein (info ci)) ci ud
    put $ (Database $ Map.insert user nud db)

lookupQ :: A.UserID -> Query Database (Either ServerError [CharityInfo])
lookupQ key = runErrorT $ do
   (Database db) <- ask
   let values = snd . unzip . Map.toList
   checkMaybe UserDoesntExist $ liftM values $ Map.lookup key db

$(makeAcidic ''Database ['updateU, 'lookupQ, 'deleteU])

lookupInfo :: (MonadIO m, MonadError ServerError m) => AcidState Database -> A.UserID -> m [CharityInfo]
lookupInfo db uid = A.rethrow $ query db (LookupQ uid)

updateInfo :: AcidState Database -> A.UserID -> CharityInfo -> IO ()
updateInfo db uid ci = update db (UpdateU uid ci)

deleteInfo :: AcidState Database -> A.UserID -> Ein -> IO ()
deleteInfo db uid cein = update db (DeleteU uid cein)
