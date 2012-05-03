{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell #-}
module Data.Product where

import Data.Login                            ( Identity)
import Data.Paypal                           ( ProductID )
import Data.Data                             ( Typeable, Data )
import Data.Distribution                     ( Distribution )
import Data.IxSet
import Data.SafeCopy

data ProductType = UserSubscription Identity
                 | OneTime [Distribution]
               deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''ProductType)
data Product = Product { pid :: ProductID, ptype ::  ProductType }
             deriving (Eq, Ord, Show, Data, Typeable)

instance Indexable Product where
    empty = ixSet [ ixFun $ \ci -> [ pid ci ]
                  ]
$(deriveSafeCopy 0 'base ''Product)

type ProductDB = IxSet Product

