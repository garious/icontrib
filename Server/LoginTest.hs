{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Login                       as L
import qualified Data.Login                  as L
import qualified DB                          as DB

import TestUtil

main :: IO ()
main = do
    db <- DB.emptyMemoryDB
    addIdentityTests db
    cookieTests db

uid :: L.Identity
uid = L.Identity "hello"

addIdentityTests :: DB.Database -> IO ()
addIdentityTests db = do
    assertEqM   "listIdentities"    (L.listIdentities db)                       []              -- No users
    assertFailM "login empty"       (L.loginToToken db uid "world")           ("DoesntExist")   -- User doesn't exist
    assertFailM "add bad user"      (L.addIdentity db (L.Identity "") "")     ("BadUsername")   -- Bad user name
    assertEqM   "added user"        (L.addIdentity db uid "world")            ()                -- Add user named 'hello'
    assertEqM   "listIdentities2"   (L.listIdentities db)                     [uid]             -- User in DB 
    assertFailM "added user"        (L.addIdentity db uid "again")            ("AlreadyExists") -- User 'hello' already exists

cookieTests :: DB.Database -> IO ()
cookieTests db = do
    assertFailM "login bad password" (L.loginToToken db uid "badpassword") ("BadPassword")   -- Bad password for existing user

    goodcookie <- (L.loginToToken db uid "world")  -- Good password for existing user

    assertFailM "tokenToIdentity bad cookie"  (L.tokenToIdentity db (L.Token "randomnonsense")) ("BadToken")  -- User lookup with bad cookie
    assertEqM   "tokenToIdentity good cookie" (L.tokenToIdentity db goodcookie)                 (uid)         -- User lookup with good cookie

    L.clearIdentityTokens db uid                                                                        -- Remove cookie
    assertFailM "tokenToIdentity good cookie" (L.tokenToIdentity db goodcookie)       ("BadToken")      -- User lookup after cookie removed


