{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell #-}
module Data.Stats where

import Data.Data                             ( Typeable, Data )
import Data.Distribution
import Data.SafeCopy


data Stats = Stats { centsDonated :: Double
                   , distribution :: [Distribution]
                   , firstName :: String
                   , lastName :: String
                   , imageUrl :: String
                   }
              deriving (Eq, Ord, Show, Data, Typeable)

empty :: Stats
empty = Stats 0 [] "icontrib.org" "community" "/Skin/logo_thumb.png"

$(deriveSafeCopy 0 'base ''Stats)
