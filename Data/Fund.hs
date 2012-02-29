{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fspec-constr-count=2 -fno-warn-orphans #-}
module Data.Fund where

import Data.Data                             ( Typeable, Data )

import Data.SafeCopy

data Fund = Fund { name   :: String 
                 , labels :: [String]
                 }
          deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Fund)


