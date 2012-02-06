{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Data.ServerError where

import Control.Monad.Error
import Data.SafeCopy
import Data.Data

data ServerError = UserAlreadyExists
                 | UserDoesntExist
                 | BadUsername
                 | BadPassword
                 | BadCookie
                 | PasswordsDontMatch
                 | CookieDecode
                 | JSONParseError String
                 | JSONDecodeError String
                 | RecordMergeError
                 | NoBody
                 | InternalError
                 deriving (Data, Typeable, Eq, Show)

$(deriveSafeCopy 0 'base ''ServerError)

instance Error ServerError


