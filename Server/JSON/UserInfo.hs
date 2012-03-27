{-# LANGUAGE DeriveDataTypeable #-}
module JSON.UserInfo where

import Data.Data                             ( Data, Typeable )

data UserInfo = UserInfo { owner            :: Identity
                         , firstName        :: String
                         , lastName         :: String
                         , phone            :: String
                         , email            :: String
                         , imageUrl         :: String
                         , centsDonated     :: Int
                         , alignedDonated   :: Int
                         , alignedUsers     :: [Identity]
                         , distribution     :: [Distribution]
                         , funds            :: [Fund]
                         }
              deriving (Eq, Ord, Show, Data, Typeable)

