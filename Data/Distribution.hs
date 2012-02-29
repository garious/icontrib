{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fspec-constr-count=2 -fno-warn-orphans #-}
module Data.Distribution where

import Data.Data                             ( Typeable, Data )
import Data.CharityInfo
import Data.SafeCopy

data Distribution = Distribution { name   :: String 
                                 , cid    :: CharityID
                                 , shares :: Int
                                 , labels :: [String]
                                 }
                   deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Distribution)


