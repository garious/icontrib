{-# LANGUAGE DeriveDataTypeable #-}
module Data.JSON where

import Data.Data                             ( Data, Typeable )

data UserLogin = UserLogin { email :: String
                           , password :: String
                           }
               deriving (Eq, Ord, Show, Data, Typeable)

type UserIdentity = String
