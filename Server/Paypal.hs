module Paypal where

import Data.Time.Clock                       ( UTCTime, getCurrentTime )
import Control.Applicative                   ( (<|>) )
import Happstack.Server                      ( lookRead )
import Data.Transaction                      ( Transaction )

sandbox = "https://api-3t.sandbox.paypal.com/nvp"
live    = "https://api-3t.paypal.com/nvp"

usrname     = "sdk-three_api1.sdk.com"
password    = "QFZCWN5HZM8VBG7Q"
signature   = "A‑IzJhZZjhg29XQ2qnhapuwxIDzyAZQ92FRP5dqBzVesOkzbdUONzmOU"

getTransactionHistory = do
    utctime <- getCurrentTime
    [ ("METHOD", "TransactionSearch")
    , ("STARTDATE", utctime)
    ]

istener = loop 0
    where
        loop ii = do
            trans <- (liftM Just $ parse ii) <|> Nothing
            case (trans) of
                (Just tt) = liftM ((:) tt) $ loop (ii + 1)
                Nothing -> return []
        parse nn = do
            let lookReadN ss = lookRead $ ss ++ (show nn)
            utctime <- lookReadN "L_TIMESTAMP"
            tmz     <- lookReadN "L_TIMEZONE"
            ttype   <- lookReadN "L_TYPE"     
            email   <- lookReadN "L_EMAIL"     
            name    <- lookReadN "L_NAME"
            tid     <- lookReadN "L_TRANSACTIONID"
            status  <- lookReadN "L_STATUS"
            amt     <- lookReadN "L_AMT"
            fee     <- lookReadN "L_FEEAMT"
            net     <- lookReadN "L_NETAMT"
            return $ Transaction utctime tmz ttype email name tid status amt fee net


