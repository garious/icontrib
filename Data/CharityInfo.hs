{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Data.CharityInfo where

import Data.Data                             ( Data, Typeable )
import qualified Data.Map                    as Map
import qualified Account                     as A
import Data.SafeCopy


data PointOfContact = PointOfContact { firstName :: String 
                                     , lastName :: String
                                     , phone :: String
                                     , email :: String
                                     }
                    deriving (Show, Typeable, Data, Eq)

$(deriveSafeCopy 0 'base ''PointOfContact)

data OrganizationInfo = OrganizationInfo { ein :: String
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

   
data Database = Database !(Map.Map A.UserID CharityInfo)
$(deriveSafeCopy 0 'base ''Database)


