module Query.Paypal where

icontribPaypalAddress :: String
icontribPaypalAddress = "paypal@icontrib.org"

getM = liftM ipnMessages . get
putM vv = get >>= (\ db -> put  db { ipnMessages = vv })

getP = liftM payments . get
putP vv = get >>= (\ db -> put  db { payments = vv })

clearUserDeposit :: Payment -> Update DB (Either String ())
clearUserDeposit pp = do
    

clearDeposit :: Payment -> Update DB ()
clearDeposit pp =
        do { isUserPaymentAddress (payer_email pp)
           ; clearUserDeposit pp }
    <|> do { isCharityPaymentAddress (payer_email pp)
           ; clearCharityDeposit pp }
    
clear :: Payment -> Update DB (Either String ())
clear pp
    | (reciever_email pp) == icontribPaypalAddress = clearDeposit pp
    | (payer_email pp) == icontribPaypalAddress = clearWithdraow pp
    | otherwise = throwError $ "unknown reciever and sender payer email" ++ (show pp)

clearPayment :: Payment -> Update DB (Either String ())
clearPayment pp = runErrorT $ do
    getP >>= (\ db -> (getOne db @* [(txn_id pp)]) `nothingOr` (throwError "duplicate payment"))
    getP >>= (\ db -> putP (IxSet.insert payment db))
    clear pp

clearValidatedPayment :: IPNMessage -> Payment -> Update DB (Either String ())
clearValidatedPayment msg payment = do
    removeIPN msg
    clearPayment payment

currentBalance :: Email -> Update DB Cents 
currentBalance email = do
    dests <- bdb $ db @* [(map Destination email)] 
    let 
            fromDest (Destination dst) = dst
            origins = map (Origin . fromDest) dests
    origins <- bdb $ db @* [(map Origin emails)]
    return $ (sum $ map cents dests) - (sum $ map cents origins)

