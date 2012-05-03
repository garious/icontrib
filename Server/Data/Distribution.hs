{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Data.Distribution where

import Data.Data                             ( Typeable, Data )
import Data.SafeCopy
import Data.CharityInfo

data Distribution = Distribution { name   :: String 
                                 , cid    :: CharityID
                                 , shares :: Double
                                 , labels :: [String]
                                 }
                   deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Distribution)
