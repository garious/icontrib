{-# LANGUAGE FlexibleContexts, OverloadedStrings, GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell #-}
module Data.Paypal where
import Data.Time.Format                      ( parseTime )
import Char                                  ( toLower )
import Data.Time.Clock                       ( UTCTime )
import System.Locale                         ( defaultTimeLocale )
import Data.Data                             ( Data, Typeable )
import Control.Monad                         ( when )
import qualified Text.Email.Validate         as E
import SiteError
import Data.IxSet
import Data.SafeCopy

newtype Cents = Cents Int
              deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Cents)

data Email = Email { localPart :: String, domainPart :: String}
           deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Email)

newtype TransactionID = TransactionID String
                      deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''TransactionID)

data Payment = Payment { reciever_email :: Email
                       , payer_email    :: Email
                       , payment_fee    :: Cents
                       , payment_gross  :: Cents
                       , payment_date   :: UTCTime
                       , txn_id         :: TransactionID 
                       }
              deriving (Eq, Ord, Show, Data, Typeable)
instance Indexable Payment where
    empty = ixSet [ ixFun $ \ix -> [(txn_id ix)]
                  ]

$(deriveSafeCopy 0 'base ''Payment)
type PaymentDB = IxSet Payment

data IPNMessage = IPNMessage { urlParams :: [(String,String)] }
                  deriving (Eq, Ord, Show, Data, Typeable)

instance Indexable IPNMessage where
    empty = ixSet [ ixFun $ \ci -> (urlParams ci)
                  ]
$(deriveSafeCopy 0 'base ''IPNMessage)
type IPNMessageDB = IxSet IPNMessage


newtype ReceiverAddress = ReceiverAddress Email
                        deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''ReceiverAddress)
newtype SenderAddress = SenderAddress Email
                      deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''SenderAddress)

data Deposit = Deposit { sender :: SenderAddress
                       , receiver :: ReceiverAddress 
                       , time :: UTCTime
                       , cents :: Cents
                       }
             deriving (Eq, Ord, Show, Data, Typeable)

instance Indexable Deposit where
    empty = ixSet [ ixFun $ \ix -> [(sender ix)]
                  , ixFun $ \ix -> [(receiver ix)]
                  ]

$(deriveSafeCopy 0 'base ''Deposit)
type DepositDB = IxSet Deposit

class Parse a where
    parse :: MonadError String m => String -> m a

data PaymentStatus = Completed

downcase :: [Char] -> [Char]
downcase = map toLower

instance Parse PaymentStatus where
    parse str
        | (downcase str) == "completed" = return $ Completed
    parse str = throwError $ "unexpected payment status: " ++ str

instance Parse UTCTime where
    parse str = parseTime defaultTimeLocale "%H:%M:%S %b %d, %Y %Z" str `justOr` (throwError $ "couldn't parse time string" ++ str)

instance Parse Email where
    parse str = do 
        case (E.validate $ str) of
            (Left er)   -> throwError (show er)
            (Right vv)  -> return (Email (E.localPart vv) (E.domainPart vv))

instance Parse Cents where
    parse str = do
        let fl :: Double
            fl = (read str) * 100
            ceiling' :: Double -> Int
            ceiling' aa = ceiling (toRational aa)
            floor' :: Double -> Int
            floor' aa = floor (toRational aa)
        when ((floor' fl) /= (ceiling' fl)) $ throwError $ "float parsing error to cents" ++ str
        return $ Cents (floor' fl)

