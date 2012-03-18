{-# LANGUAGE DeriveDataTypeable #-}
module Data.Popular where

import Data.Data                     ( Data, Typeable )
import Data.CharityInfo              ( CharityID )

data Popular = Popular { cid :: CharityID
                       , name :: String
                       }
             deriving (Eq, Ord, Show, Data, Typeable)


