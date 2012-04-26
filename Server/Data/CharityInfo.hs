{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell #-}
module Data.CharityInfo where

import Data.Login
import Data.Data                             ( Data, Typeable )
import Data.IxSet
import Data.SafeCopy
import Data.Paypal (Email(..))

newtype Ein         = Ein       String    deriving (Eq, Ord, Show, Data, Typeable, SafeCopy)
newtype CharityID   = CharityID String    deriving (Eq, Ord, Show, Data, Typeable, SafeCopy)

data CharityInfo = CharityInfo { owner :: Identity
                               , ein :: Ein
                               , organizationName :: String
                               , companyWebsite :: String
                               , cid :: CharityID
                               , imageUrl :: String
                               , mission :: String
                               , paymentAddress :: Email
                               }
                 deriving (Eq, Ord, Show, Data, Typeable)

empty :: CharityInfo
empty = CharityInfo (Identity "") (Ein "") "" "" (CharityID "") "" "" (Email "paypal@charity.com")


instance Indexable CharityInfo where
    empty = ixSet [ ixFun $ \ci -> [ ein ci ]
                  , ixFun $ \ci -> [ cid ci ]
                  , ixFun $ \ci -> [ owner ci ]
                  ]
$(deriveSafeCopy 0 'base ''CharityInfo)

type CharityInfoDB = IxSet CharityInfo
