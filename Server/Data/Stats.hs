{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell #-}
module Data.Stats where

import Data.Data                             ( Typeable, Data )
import Data.Distribution
import Data.SafeCopy


data Stats = Stats { centsDonated :: Double
                   , distribution :: [Distribution]
                   }
              deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Stats)
