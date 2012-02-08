{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Data.CharityInfo where

import Data.Data                             ( Data, Typeable )
import qualified Data.Map                    as Map
import qualified Account                     as A
import Data.SafeCopy

type Ein = String
data PointOfContact = PointOfContact { firstName :: String 
                                     , lastName :: String
                                     , phone :: String
                                     , email :: String
                                     }
                    deriving (Show, Typeable, Data, Eq)

$(deriveSafeCopy 0 'base ''PointOfContact)

data OrganizationInfo = OrganizationInfo { ein :: Ein
                                         , organizationName :: String
                                         , companyWebsite :: String
                                         }
                             deriving (Show, Typeable, Data, Eq)

$(deriveSafeCopy 0 'base ''OrganizationInfo)

data CharityInfo = CharityInfo { info :: OrganizationInfo 
                               , poc :: PointOfContact
                               }
                 deriving (Show, Typeable, Data, Eq)

$(deriveSafeCopy 0 'base ''CharityInfo)

type CharityMap = Map.Map Ein CharityInfo 
data Database = Database !(Map.Map A.UserID CharityMap)
$(deriveSafeCopy 0 'base ''Database)


