module Main where

import ServerError                           ( ServerError(..) )
import Char                                  ( ord )
import Data.Acid.Memory                      ( openMemoryState )
import Control.Monad.Error                   ( runErrorT, ErrorT )
import Data.Acid                             ( AcidState )
import qualified Account                     as A
import qualified Data.ByteString.Lazy        as B

main :: IO ()
main = do
    db <- openMemoryState A.empty
    addUserTests db
    cookieTests db

addUserTests :: AcidState A.Database -> IO ()
addUserTests db = do
    assertEqM      "listUsers"    (A.listUsers db)                                   []                       -- No users
    assertEqErrorT "login empty"  (A.loginToCookie db (toB "hello") (toB "world"))   (Left UserDoesntExist)   -- User doesn't exist
    assertEqErrorT "add bad user" (A.addUser db (toB "") (toB ""))                   (Left BadUsername)       -- Bad user name
    assertEqErrorT "added user"   (A.addUser db (toB "hello") (toB "world"))         (Right ())               -- Add user named 'hello'
    assertEqM      "listUsers2"   (A.listUsers db)                                   [toB "hello"]            -- User in DB 
    assertEqErrorT "added user"   (A.addUser db (toB "hello") (toB "again"))         (Left UserAlreadyExists) -- User 'hello' already exists

cookieTests :: AcidState A.Database -> IO ()
cookieTests db = do
    assertEqErrorT "login bad password" (A.loginToCookie db (toB "hello") (toB "badpassword")) (Left BadPassword)   -- Bad password for existing user

    goodcookie <- assertRightErrorT "loginToCookie good password" (A.loginToCookie db (toB "hello") (toB "world"))  -- Good password for existing user

    assertEqErrorT "cookieToUser bad cookie"  (A.cookieToUser db (toB "randomnonsense")) (Left BadCookie)           -- User lookup with bad cookie
    assertEqErrorT "cookieToUser good cookie" (A.cookieToUser db goodcookie)             (Right (toB "hello"))      -- User lookup with good cookie

    A.clearUserCookie db (toB "hello")                                                                              -- Remove cookie
    assertEqErrorT "cookieToUser good cookie" (A.cookieToUser db goodcookie)             (Left BadCookie)           -- User lookup after cookie removed

-- String to ByteString
toB :: String -> B.ByteString
toB = B.pack . map (fromIntegral . ord)

-- Assert an ErrorT action returns the expected value
assertEqErrorT :: (Eq e, Eq a) => String -> ErrorT e IO a -> Either e a -> IO ()
assertEqErrorT msg transaction expected = assertEqM msg (runErrorT transaction) expected


-- Assert an ErrorT action returns any value that is Right.  
-- Return the value inside the Right constructor
assertRightErrorT :: String -> ErrorT e IO a -> IO a
assertRightErrorT msg transaction = do
    actual <- runErrorT transaction
    assert msg (isRight actual)
    let (Right x) = actual
    return x

-- Return True if Right
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _)  = False


-- Assert a monadic returns the expected value
assertEqM :: (Eq a) => String -> IO a -> a -> IO ()
assertEqM msg actualM expected = do
    actual <- actualM
    assert msg (actual == expected)

-- Assert the argument is true or bail out with the given error message
assert :: String -> Bool -> IO ()
assert msg False = error msg
assert _   True  = return ()

