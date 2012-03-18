module TestUtil where

import Control.Monad.Error                   ( runErrorT, ErrorT )
import Control.Monad ( liftM )
import qualified IO as IO
-- Assert an ErrorT action returns the expected value
assertEqErrorT :: (Show e, Show a, Eq e, Eq a, Monad m) => String -> ErrorT e m a -> Either e a -> m ()
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
assertEqM :: (Eq a, Show a, Monad m) => String -> m a -> a -> m ()
assertEqM msg actualM expected = do
    actual <- actualM
    assert (msg ++ ": " ++ (show actual) ++ " /= " ++ (show expected)) (actual == expected)

-- Assert the argument is true or bail out with the given error message
assert :: Monad m => String -> Bool -> m ()
assert msg False = error msg
assert _   True  = return ()

assertFailM :: (Show a1, Eq a1) => [Char] -> IO a1 -> String -> IO ()
assertFailM msg actualM expected = do
    let
        catcher ee = if (IO.isUserError ee) then return (Left (IO.ioeGetErrorString ee)) else ioError ee
        fromLeft (Left aa) = aa
        fromLeft _ = error "fromLeft"
    actual <- (liftM Right (actualM)) `catch` catcher
    assert (msg ++ ": " ++ (show (fromLeft actual)) ++ " /= " ++ (show expected)) (actual == (Left expected))

