module Data.Transaction where

import Data.Time.Clock                       ( UTCTime, getCurrentTime )
import Data.Time.LocalTime                   ( TimeZone )
import Text.Email.Validate                   ( EmailAddress, validate )

newtype Cents = Cents Int
newtype Email = Email EmailAddress

data Transaction = Transaction { timestamp :: UTCTime
                               , timezone  :: TimeZone
                               , ttype :: String
                               , email :: Email
                               , name :: String
                               , tid :: String 
                               , status :: String
                               , amt :: Cents
                               , fee :: Cents
                               , net :: Cents
                               }

instance Read Email where
    read str = fromRight validate $ str
        where
            fromRight (Left msg) = error msg
            fromRight (Right addr) = Email addr

instance Read Cents where
    read str = 
        let fl :: Float 
            fl = (read str) * 100
            valid
                | (floor fl) /= fl = error $ "Read: invalid Cents: " ++ show str
                | otherwise = Cents (fromRational str) 
        in  valid


