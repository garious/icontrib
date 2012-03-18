{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, TemplateHaskell #-}
module Data.DB where

import Data.Data                               ( Data, Typeable )
import qualified Data.Login                    as L
import qualified Data.CharityInfo              as C
import qualified Data.UserInfo                 as U
import Data.SafeCopy

data DB = DB { logins :: L.LoginDB 
             , charities :: C.CharityInfoDB 
             , userInfos :: U.UserInfoDB
             }
        deriving (Eq, Ord, Show, Data, Typeable)


$(deriveSafeCopy 0 'base ''DB)

