{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies, FlexibleContexts #-}
module Query.Paypal(clearValidatedPaymentU, currentBalanceQ) where

import Control.Monad.State                   ( MonadState )
import Query.UserInfo                        ( userDistributionMQ )
import Data.Distribution                     ( Distribution )
import Query.CharityInfo                     ( charityByIDMQ' )

import Data.IxSet                            ( (@*) )
import qualified Data.CharityInfo            as C
import Data.Distribution                     ( shares, cid )
import qualified Data.IxSet                  as IxSet
import Data.Paypal
import Data.Product

import Data.Acid
import SiteError
import Data.DB

icontribPaypalAddress :: String
icontribPaypalAddress = "paypal@icontrib.org"

removeIPN :: IPNMessage -> Update DB ()
removeIPN mm = do
    db <- getM
    putM $ IxSet.delete mm db 

distributePayment :: (MonadError String m, MonadState DB m) => Payment -> [Distribution] -> m ()
distributePayment pp dists = do
    chars <- charityByIDMQ' $ map cid dists
    let 
        td = fromIntegral
        total = sum $ map shares dists
        moneys = ((payment_gross pp) - (payment_fee pp))

        depAmnt' :: Distribution -> Double
        depAmnt' dd = ((td $ shares dd) / (td total)) * (td $ fromCents moneys)

        depAmnt :: Distribution -> Cents
        depAmnt dd = Cents $ floor $ depAmnt' dd

        remainder :: Cents
        remainder = moneys - (sum (map depAmnt dists))

        toDeposit :: Distribution -> C.CharityInfo -> Deposit 
        toDeposit dd cc
            | dd == (head dists) = Deposit (SenderAddress $ payer_email pp) 
                                           (ReceiverAddress $ C.paymentAddress cc) 
                                           (payment_date pp) ((depAmnt dd) + remainder)
        toDeposit dd cc = Deposit (SenderAddress $ payer_email pp) 
                                  (ReceiverAddress $ C.paymentAddress cc) 
                                  (payment_date pp) (depAmnt dd)
    deps <- getD
    putD $ IxSet.union deps (IxSet.fromList $ zipWith toDeposit dists chars) 
            
clearUserDeposit :: (MonadError String m, MonadState DB m) => Payment -> Product -> m ()
clearUserDeposit pp (Product _ (OneTime dists)) = distributePayment pp dists
clearUserDeposit pp (Product _ (UserSubscription uid)) = do
    dists <- userDistributionMQ uid
    distributePayment pp dists

    
clear :: (MonadError String m, MonadState DB m) => Payment -> m ()
clear pp
    | (show $ reciever_email pp) == icontribPaypalAddress = do
            pdb <- getPR
            prod <- (IxSet.getOne $ pdb @* [(custom pp)]) `justOr` (throwError "no product for payment")
            clearUserDeposit pp prod
    | otherwise = throwError $ "unknown reciever and sender payer email" ++ (show pp)

clearPayment :: (MonadError String m, MonadState DB m) => Payment -> m ()
clearPayment pp = do
    db <- getP 
    _ <- (IxSet.getOne $ db @* [(txn_id pp)]) `nothingOr` (throwError "duplicate payment")
    putP (IxSet.insert pp db)
    clear pp

clearValidatedPaymentU :: IPNMessage -> Payment -> Update DB (Either String ())
clearValidatedPaymentU msg payment = runErrorT $ do
    lift $ removeIPN msg
    clearPayment payment

currentBalanceQ :: Email -> Query DB Cents 
currentBalanceQ email = do
    db <- askD
    let deps = IxSet.toList $ db @* [ReceiverAddress email] 
    return $ (sum $ map cents deps)

