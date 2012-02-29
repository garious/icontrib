{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fspec-constr-count=2 -fno-warn-orphans #-}
module CharityInfo where

import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )
import Control.Monad.Error                   ( runErrorT, MonadError )
import Control.Monad.IO.Class                ( MonadIO, liftIO )
import Data.IxSet                            ( (@*) )
import qualified Data.IxSet                  as IxSet
import Control.Applicative                   ( (<|>), (*>) )
import qualified Account                     as A
import Data.Acid
import ServerError
import Data.CharityInfo

empty :: Database
empty = Database IxSet.empty

deleteByEinU :: A.UserID -> Ein -> Update Database (Either ServerError ())
deleteByEinU uid cien = runErrorT $ do
    (Database db) <- get
    cis <- (IxSet.getOne $ db @* [uid] @* [cien]) `justOr` doesntExist
    put $ Database (IxSet.delete cis db)

updateU :: CharityInfo -> Update Database (Either ServerError ())
updateU ci =  runErrorT $ do
    (Database db) <- get
    let 
        mci = IxSet.getOne $ db @* [ein ci]
        notExist mm = mm `nothingOr` einAlreadyExists
        belongs (Just jci)
            | (owner jci) == (owner ci) = return ()
        belongs _                       = badUsername
        sameID (Just jci)
            | (cid jci) == (cid ci)  = return ()
        sameID _                     = badUsername
        idNotTaken (Just jci) = notExist $ IxSet.getOne $ db @* [cid jci]
        idNotTaken _          = return ()
    (notExist mci) <|> (belongs mci *> (sameID mci <|> idNotTaken mci))
    put $ Database (IxSet.insert ci db)

lookupByOwnerQ :: A.UserID -> Query Database [CharityInfo]
lookupByOwnerQ uid = do
   (Database db) <- ask
   return $ IxSet.toList $ db @* [uid]

$(makeAcidic ''Database ['updateU, 'lookupByOwnerQ, 'deleteByEinU])

lookupByOwner :: MonadIO m => AcidState Database -> A.UserID -> m [CharityInfo]
lookupByOwner db uid = liftIO $ query db (LookupByOwnerQ uid)

updateInfo :: (MonadIO m, MonadError ServerError m) => AcidState Database -> A.UserID -> CharityInfo -> m ()
updateInfo db uid ci
    | uid /= (owner ci) = badUsername
    | otherwise = A.rethrow $ update db (UpdateU ci)

deleteByEin :: (MonadIO m, MonadError ServerError m) => AcidState Database -> A.UserID -> Ein -> m ()
deleteByEin db uid cein = A.rethrow $ update db (DeleteByEinU uid cein)

