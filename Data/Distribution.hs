{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fspec-constr-count=2 -fno-warn-orphans #-}
module Data.Distribution where

import Data.Data                             ( Typeable, Data )

import Data.SafeCopy

data Distribution = Distribution { name   :: String 
                                 , url    :: String
                                 , shares :: Int
                                 , labels :: [String]
                                 }
                   deriving (Show, Typeable, Data, Eq)

$(deriveSafeCopy 0 'base ''Distribution)


