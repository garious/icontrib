{-# LANGUAGE UndecidableInstances, GeneralizedNewtypeDeriving, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Data.CharityInfo where

import Data.Data                             ( Data, Typeable )
import qualified Account                     as A
import Data.IxSet
import Data.SafeCopy

newtype Ein         = Ein       String    deriving (Eq, Ord, Show, Data, Typeable, SafeCopy)
newtype CharityID   = CharityID String    deriving (Eq, Ord, Show, Data, Typeable, SafeCopy)

data CharityInfo = CharityInfo { owner :: A.UserID
                               , ein :: Ein
                               , name :: String
                               , companyWebsite :: String
                               , cid :: CharityID
                               , imageUrl :: String
                               , mission :: String
                               }
                 deriving (Eq, Ord, Show, Data, Typeable)


instance Indexable CharityInfo where
    empty = ixSet [ ixFun $ \ci -> [ ein ci ]
                  , ixFun $ \ci -> [ cid ci ]
                  , ixFun $ \ci -> [ owner ci ]
                  ]
$(deriveSafeCopy 0 'base ''CharityInfo)

newtype Database = Database (IxSet CharityInfo)
                 deriving (Data, Typeable)
$(deriveSafeCopy 0 'base ''Database)
