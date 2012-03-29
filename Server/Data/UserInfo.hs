{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell #-}
module Data.UserInfo where

import Data.Data                             ( Typeable, Data )
import Data.Login
import qualified Data.ByteString.Lazy        as BL

import Data.IxSet
import Data.SafeCopy
import Data.Distribution
import Data.Fund


data UserInfo = UserInfo { owner            :: Identity
                         , firstName        :: String
                         , lastName         :: String
                         , phone            :: String
                         , email            :: BL.ByteString
                         , imageUrl         :: String
                         , centsDonated     :: Int
                         , alignedDonated   :: Int
                         , alignedUsers     :: [Identity]
                         , distribution     :: [Distribution]
                         , funds            :: [Fund]
                         }
              deriving (Eq, Ord, Show, Data, Typeable)

empty :: UserInfo
empty = UserInfo (Identity "") "" "" "" "" "" 0 0 [] [] []

instance Indexable UserInfo where
    empty = ixSet [ ixFun $ \ci -> [ owner ci ]
                  ]
$(deriveSafeCopy 0 'base ''UserInfo)

type UserInfoDB = IxSet UserInfo
