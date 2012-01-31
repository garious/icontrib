module TestUtil where

import qualified Data.ByteString.Lazy        as B
import Char                                  ( ord )
import Control.Monad.Error                   ( runErrorT, ErrorT )

-- String to ByteString
toB :: String -> B.ByteString
toB = B.pack . map (fromIntegral . ord)

-- Assert an ErrorT action returns the expected value
assertEqErrorT :: (Show e, Show a, Eq e, Eq a) => String -> ErrorT e IO a -> Either e a -> IO ()
assertEqErrorT msg transaction expected = assertEqM msg (runErrorT transaction) expected


-- Assert an ErrorT action returns any value that is Right.  
-- Return the value inside the Right constructor
assertRightErrorT :: (Show e, Show a) => String -> ErrorT e IO a -> IO a
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
assertEqM :: (Eq a, Show a) => String -> IO a -> a -> IO ()
assertEqM msg actualM expected = do
    actual <- actualM
    assert (msg ++ ": " ++ (show actual) ++ " /= " ++ (show expected)) (actual == expected)

-- Assert the argument is true or bail out with the given error message
assert :: String -> Bool -> IO ()
assert msg False = error msg
assert _   True  = return ()

