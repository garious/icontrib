{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fspec-constr-count=2 -fno-warn-orphans #-}
module Data.UserInfo where

import Data.Data                             ( Typeable, Data )
import qualified Account                     as A

import Data.SafeCopy
import Data.Distribution
import Data.Fund


data UserInfo = UserInfo { uid              :: A.UserID 
                         , firstName        :: String
                         , lastName         :: String
                         , imageUrl         :: String
                         , centsDonated     :: Int
                         , alignedDonated   :: Int
                         , alignedUsers     :: [A.UserID]
                         , distribution     :: [Distribution]
                         , funds            :: [Fund]
                         }
              deriving (Show, Typeable, Data, Eq)

$(deriveSafeCopy 0 'base ''UserInfo)


