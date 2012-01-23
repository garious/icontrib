{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fspec-constr-count=2 -fno-warn-orphans #-}
module CharityInfo where

import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )
import Control.Monad.Error                   ( runErrorT, MonadError )
import Data.Typeable                         ()
import Data.Typeable                         (Typeable)
import Control.Monad.IO.Class                ( MonadIO )
import qualified Text.JSON                   as JS
import qualified Data.Map                    as Map
import qualified Account                     as A

import Text.JSON
import Data.Derive.JSON
import Data.DeriveTH
import Data.Acid
import Data.SafeCopy
import ServerError

--authenticated user for charity account
type CharityID    = A.UserID

data PointOfContact = PointOfContact { firstName :: String 
                                     , lastName :: String
                                     , phone :: String
                                     , email :: String
                                     }
                    deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''PointOfContact)
$(derive makeJSON ''PointOfContact)

data OrganizationInfo = OrganizationInfo { ein :: String
                                         , organizationName :: String
                                         , companyWebsite :: String
                                         }
                             deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''OrganizationInfo)
$(derive makeJSON ''OrganizationInfo)

data CharityInfo = CharityInfo { info :: OrganizationInfo 
                               , poc :: PointOfContact
                               }
                 deriving (Show, Typeable)
new :: CharityInfo
new = CharityInfo (OrganizationInfo "ein" "name" "website") (PointOfContact "first" "last" "phone" "email")

$(deriveSafeCopy 0 'base ''CharityInfo)
$(derive makeJSON ''CharityInfo)

   
data Database = Database !(Map.Map CharityID CharityInfo)
$(deriveSafeCopy 0 'base ''Database)

empty :: Database
empty = Database Map.empty

--todo: do compare and exchange so you dont merge inside the db lock
updateU :: CharityID -> CharityInfo -> Update Database ()
updateU key val =  do
   (Database db) <- get
   put $ (Database $ Map.insert key val db)

lookupQ :: CharityID -> Query Database (Either ServerError CharityInfo)
lookupQ key = runErrorT $ do
   (Database db) <- ask
   checkMaybe UserDoesntExist $ Map.lookup key db


$(makeAcidic ''Database ['updateU, 'lookupQ])

lookupInfo :: (MonadIO m, MonadError ServerError m) => AcidState Database -> CharityID -> m CharityInfo
lookupInfo db cid = A.rethrow $ query db (LookupQ cid)

updateInfo :: AcidState Database -> CharityID -> CharityInfo -> IO ()
updateInfo db cid str = update db (UpdateU cid str)
