{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Data.Fund where

import Data.Data                             ( Typeable, Data )

import Data.SafeCopy

data Fund = Fund { name   :: String 
                 , labels :: [String]
                 }
          deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Fund)


