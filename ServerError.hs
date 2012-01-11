{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fspec-constr-count=2 -fno-warn-orphans #-}
module ServerError where

import Control.Monad.Error
import Data.SafeCopy
import Data.Data
import Text.JSON
import Data.Derive.JSON
import Data.DeriveTH

data ServerError = UserAlreadyExists
                 | UserDoesntExist
                 | BadUsername
                 | BadPassword
                 | BadCookie
                 | PasswordsDontMatch
                 | CookieDecode
                 | JSONDecodeError
                 | RecordMergeError
                 | InternalError
                 deriving (Data, Typeable, Eq, Show)

$(deriveSafeCopy 0 'base ''ServerError)
$(derive makeJSON ''ServerError)

instance Error ServerError


catchOnly :: (MonadError e m, Eq e) => e -> m a -> m a -> m a
catchOnly ee ra rb = ra `catchError` (\ er -> case(er == ee) of 
                                                   True -> rb
                                                   _    -> throwError er)



checkMaybe :: MonadError e m => e -> Maybe a -> m a
checkMaybe ee Nothing = throwError ee
checkMaybe _ (Just aa) = return aa

