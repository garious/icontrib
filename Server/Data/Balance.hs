module Data.Balance where

import Data.Paypal

data Balance = Balance { origin :: Origin
                       , destination :: Destination
                       , cents :: Cents 
                       , time :: UTCTime
                       }
             deriving (Eq, Ord, Show, Data, Typeable)

instance Indexable Balance where
    empty = ixSet [ ixFun $ \ix -> (origin ix)
                  , ixFun $ \ix -> (destination ix)
                  ]

$(deriveSafeCopy 0 'base ''Balance)
type BalanceDB = IxSet Balance

