{-# LANGUAGE NoMonomorphismRestriction #-}
module ServerError where

import Control.Monad.IO.Class                ( MonadIO, liftIO )

einAlreadyExists :: Monad m => m a
einAlreadyExists = fail "EinAlreadyExists"

alreadyExists :: Monad m => m a
alreadyExists = fail "AlreadyExists"

doesntExist :: Monad m => m a
doesntExist = fail "DoesntExist"

badUsername :: Monad m => m a
badUsername = fail "BadUsername"

badPassword :: Monad m => m a
badPassword = fail "BadPassword"

badToken :: Monad m => m a
badToken = fail "BadToken"

passwordsDontMatch :: Monad m => m a
passwordsDontMatch  = fail "PasswordsDontMatch"

cookieDecodeError :: Monad m => m a
cookieDecodeError = fail "CookieDecodeError"

jsonParseError :: Monad m => String -> m a
jsonParseError str  = fail ("JSONParseError: " ++ str)

jsonDecodeError :: Monad m => String -> m a
jsonDecodeError str = fail ("JSONDecodeError: " ++ str)

jsonMergeError :: Monad m => String -> m a
jsonMergeError str = fail ("JSONMergeError: " ++ str)

missingHTTPBody :: Monad m => m a
missingHTTPBody = fail "MissingHTTPBody"

internalError :: Monad m => m a
internalError = fail "InternalError"

justOr :: Monad m => Maybe a -> m a -> m a
justOr Nothing ee  = ee
justOr (Just aa) _ = return aa

nothingOr :: Monad m => Maybe a -> m () -> m () 
nothingOr Nothing _   = return ()
nothingOr (Just _) ee = ee

failLeftIO :: (MonadIO m) => IO (Either String b) -> m b
failLeftIO aa = do
   err <- liftIO aa
   case(err) of
      (Left ee)   -> fail ee
      (Right val) -> return val

