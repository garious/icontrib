{-# LANGUAGE NoMonomorphismRestriction #-}
module Data.ServerError where

type ServerError = String

alreadyExists :: Monad m => m a
alreadyExists = fail "AlreadyExists"

doesntExist :: Monad m => m a
doesntExist = fail "DoesntExist"

badUsername :: Monad m => m a
badUsername = fail "BadUsername"

badPassword :: Monad m => m a
badPassword = fail "BadPassword"

badCookie :: Monad m => m a
badCookie = fail "BadCookie"

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


