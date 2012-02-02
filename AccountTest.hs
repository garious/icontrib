{-# LANGUAGE OverloadedStrings #-}

module Main where

import ServerError                           ( ServerError(..) )
import Data.Acid.Memory                      ( openMemoryState )
import Data.Acid                             ( AcidState )
import qualified Account                     as A

import TestUtil

main :: IO ()
main = do
    db <- openMemoryState A.empty
    addUserTests db
    cookieTests db

addUserTests :: AcidState A.Database -> IO ()
addUserTests db = do
    assertEqM      "listUsers"    (A.listUsers db)                       []                       -- No users
    assertEqErrorT "login empty"  (A.loginToCookie db "hello" "world")   (Left UserDoesntExist)   -- User doesn't exist
    assertEqErrorT "add bad user" (A.addUser db "" "")                   (Left BadUsername)       -- Bad user name
    assertEqErrorT "added user"   (A.addUser db "hello" "world")         (Right ())               -- Add user named 'hello'
    assertEqM      "listUsers2"   (A.listUsers db)                       ["hello"]                -- User in DB 
    assertEqErrorT "added user"   (A.addUser db "hello" "again")         (Left UserAlreadyExists) -- User 'hello' already exists

cookieTests :: AcidState A.Database -> IO ()
cookieTests db = do
    assertEqErrorT "login bad password" (A.loginToCookie db "hello" "badpassword") (Left BadPassword)   -- Bad password for existing user

    goodcookie <- assertRightErrorT "loginToCookie good password" (A.loginToCookie db "hello" "world")  -- Good password for existing user

    assertEqErrorT "cookieToUser bad cookie"  (A.cookieToUser db "randomnonsense") (Left BadCookie)     -- User lookup with bad cookie
    assertEqErrorT "cookieToUser good cookie" (A.cookieToUser db goodcookie)       (Right "hello")      -- User lookup with good cookie

    A.clearUserCookie db "hello"                                                                        -- Remove cookie
    assertEqErrorT "cookieToUser good cookie" (A.cookieToUser db goodcookie)       (Left BadCookie)     -- User lookup after cookie removed


