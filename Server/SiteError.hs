{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module SiteError(MonadError
                ,throwError
                ,MonadIO
                ,liftIO
                ,lift
                ,liftM
                ,runErrorT
                ,ErrorT
                ,SiteErrorT
                ,throwLeft
                ,justOr
                ,nothingOr
                ,runErrorT_
                ,badUsername 
                ,alreadyExists 
                ,badToken 
                ,doesntExist 
                ,badPassword 
                ,badCharityID 
                ) where

import Control.Monad.IO.Class             ( MonadIO, liftIO )
import Control.Monad.Error                ( MonadError, ErrorT, throwError, runErrorT )
import Control.Monad.Trans                ( lift )
import Control.Monad                      ( liftM )
import Happstack.Server.Monads            ( ServerPartT )

type SiteErrorT a = ErrorT String (ServerPartT IO) a

badUsername :: MonadError String m => m a
badUsername = throwError "BadUsername"

alreadyExists :: MonadError String m => m a
alreadyExists = throwError "AlreadyExists"

badToken :: MonadError String m => m a
badToken = fail "BadToken"

doesntExist :: MonadError String m => m a
doesntExist = fail "DoesntExist"

badPassword :: MonadError String m => m a
badPassword = fail "BadPassword"

badCharityID :: MonadError String m => m a
badCharityID = fail "BadCharityID"

--passwordsDontMatch :: MonadError String m => m a
--passwordsDontMatch  = fail "PasswordsDontMatch"
--
--
--jsonMergeError :: MonadError String m => String -> m a
--jsonMergeError str = fail ("JSONMergeError: " ++ str)
--
--missingHTTPBody :: MonadError String m => m a
--missingHTTPBody = fail "MissingHTTPBody"
--
--internalError :: MonadError String m => m a
--internalError = fail "InternalError"
--

justOr :: Monad m => Maybe a -> m a -> m a
justOr Nothing ee  = ee
justOr (Just aa) _ = return aa

nothingOr :: Monad m => Maybe a -> m () -> m () 
nothingOr Nothing _   = return ()
nothingOr (Just _) ee = ee

throwLeft :: (MonadError e m, MonadIO m) => IO (Either e b) -> m b
throwLeft aa = do
   err <- liftIO aa
   case(err) of
      (Left ee)   -> throwError ee
      (Right val) -> return val

runErrorT_ :: Monad m => ErrorT e m a -> m ()
runErrorT_ aa = do
    _ <- runErrorT aa
    return ()

