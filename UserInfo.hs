{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fspec-constr-count=2 -fno-warn-orphans #-}
module UserInfo where

import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )
import Control.Monad.Error                   ( runErrorT, MonadError )
import Data.Function                         ( on )
import Data.List                             ( sortBy, group, sort )
import Control.Monad.IO.Class                ( MonadIO, liftIO )
import qualified Account                     as A
import Data.Generics                         ( listify )
import Data.CharityInfo                      ( CharityID(..) )

import Data.IxSet                            ( (@*) )
import qualified Data.IxSet                  as IxSet

import Data.Acid
import ServerError
import Data.UserInfo

empty :: Database
empty = Database IxSet.empty

updateU :: UserInfo -> Update Database ()
updateU ui =  do
    (Database db) <- get
    put $ Database (IxSet.insert ui db)

lookupByOwnerQ :: A.UserID -> Query Database (Either ServerError UserInfo)
lookupByOwnerQ key = runErrorT $ do
   (Database db) <- ask
   (IxSet.getOne $ db @* [key]) `justOr` doesntExist

listQ :: Query Database ([A.UserID])
listQ = do
   (Database db) <- ask
   return $ map owner $ IxSet.toList db

mostInfluentialQ :: Query Database (Either ServerError A.UserID)
mostInfluentialQ = runErrorT $ do
   (Database db) <- ask
   let 
        head' [] = doesntExist
        head' ls = return $ owner $ head ls
        influence aa = negate $ (centsDonated aa) + (alignedDonated aa)
   head' $ sortBy (compare `on` influence) $ IxSet.toList db

listCharitiesQ :: Query Database ([CharityID])
listCharitiesQ = do
    (Database db) <- ask
    let isCharityID (CharityID _) = True
    return $ listify isCharityID (IxSet.toList db)

$(makeAcidic ''Database ['updateU, 'lookupByOwnerQ, 'mostInfluentialQ, 'listQ, 'listCharitiesQ ])

mostInfluential :: (MonadIO m, MonadError ServerError m) => AcidState Database -> m A.UserID
mostInfluential db = A.rethrow $ query db MostInfluentialQ

lookupByOwner :: (MonadIO m, MonadError ServerError m) => AcidState Database -> A.UserID -> m UserInfo
lookupByOwner db cid = A.rethrow $ query db (LookupByOwnerQ cid)

updateInfo :: (MonadIO m, MonadError ServerError m) => AcidState Database -> A.UserID -> UserInfo -> m ()
updateInfo db uid ui
    | uid /= (owner ui) = badUsername
    | otherwise = liftIO $ update db (UpdateU ui)

list :: AcidState Database -> IO ([A.UserID])
list db = query db (ListQ)

popularCharities :: AcidState Database -> IO [CharityID]
popularCharities db = do 
    lst <- query db (ListCharitiesQ)
    return $ map head $ reverse $ sortBy (compare `on` length) $ group $ sort lst

