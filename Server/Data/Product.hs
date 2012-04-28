{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell #-}
module Data.Product where

import Data.Login                            ( Identity, Token )
import Data.Data                             ( Typeable, Data )
import Data.Distribution                     ( Distribution )
import Monad                                 ( ap, liftM )
import Random                                ( randomIO )
import qualified Data.ByteString.Lazy        as BL
import qualified Codec.Binary.Url            as Url
import Data.IxSet
import Data.SafeCopy

data ProductID = ProductID { invoice :: BL.ByteString
                           , name :: BL.ByteString
                           , number :: BL.ByteString
                           }
               deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''ProductID)
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

newProductID :: IO ProductID
newProductID = do
    let newid = liftM (take 127 . Url.encode) $ sequence $ repeat randomIO
    return $ ProductID `ap` newid `ap` newid `ap` newid

