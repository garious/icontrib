{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies, FlexibleContexts #-}
module Query.CharityInfo where

import Control.Monad.State                   ( get, put, MonadState )
import Control.Monad.Reader                  ( ask, MonadReader )
import Data.IxSet                            ( (@*), (@+) )
import qualified Data.IxSet                  as IxSet
import Control.Applicative                   ( (<|>), (*>) )
import Data.Acid
import SiteError
import Data.DB
import Data.Login
import Data.CharityInfo

replace :: MonadState DB m => (CharityInfoDB -> m CharityInfoDB) -> m ()
replace ff = do
    db <- get
    ll <- ff (charities db)
    put $ db { charities = ll }

use :: MonadReader DB m => (CharityInfoDB -> m b) -> m b
use ff = do
    db <- ask
    ff (charities db)

deleteCharityByEinU :: Identity -> Ein -> Update DB (Either String ())
deleteCharityByEinU uid cien = runErrorT $ replace $ \ db -> do
    cis <- (IxSet.getOne $ db @* [uid] @* [cien]) `justOr` doesntExist
    return $ IxSet.delete cis db


charityInfoU :: CharityInfo -> Update DB (Either String ())
charityInfoU ci =  runErrorT $ replace $ \ db -> do
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
    return $ IxSet.updateIx (ein ci) ci db

einAlreadyExists :: MonadError String m => m a
einAlreadyExists = fail "EinAlreadyExists"

charityByOwnerQ :: Identity -> Query DB [CharityInfo]
charityByOwnerQ uid = use $ \ db -> return $ IxSet.toList $ db @* [uid]

charityByIDQ :: [CharityID] -> Query DB [CharityInfo]
charityByIDQ cids = use $ \ db -> return $ IxSet.toList $ db @+ cids

