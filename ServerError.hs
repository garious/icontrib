module ServerError( catchOnly
                  , checkMaybe
                  , module Data.ServerError
                  ) where

import Data.ServerError
import Control.Monad.Error


catchOnly :: (MonadError e m, Eq e) => e -> m a -> m a -> m a
catchOnly ee ra rb = ra `catchError` (\ er -> case(er == ee) of 
                                                   True -> rb
                                                   _    -> throwError er)



checkMaybe :: MonadError e m => e -> Maybe a -> m a
checkMaybe ee Nothing = throwError ee
checkMaybe _ (Just aa) = return aa

