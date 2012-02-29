module ServerError( justOr
                  , nothingOr
                  , module Data.ServerError
                  ) where

import Data.ServerError


justOr :: Monad m => Maybe a -> m a -> m a
justOr Nothing ee  = ee
justOr (Just aa) _ = return aa

nothingOr :: Monad m => Maybe a -> m () -> m () 
nothingOr Nothing _   = return ()
nothingOr (Just _) ee = ee
