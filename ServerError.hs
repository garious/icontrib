{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fspec-constr-count=2 -fno-warn-orphans #-}
module ServerError where

import Control.Monad.Error
import Data.SafeCopy
import Data.Data

data ServerError = UserAlreadyExists
                 | UserDoesntExist
                 | BadPassword
                 | BadCookie
                 | PasswordsDontMatch
                 deriving (Data, Typeable, Eq, Show)

$(deriveSafeCopy 0 'base ''ServerError)

instance Error ServerError


