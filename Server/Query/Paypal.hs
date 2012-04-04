module Query.Paypal where

mdb :: MonadState DB m => (IPNMessageDB -> m IPNMessageDB) -> m ()
mdb ff = (get >>= (ff  . ipnMessages))

pdb :: MonadState DB m => (PaymentDB -> m a) -> m (a)
pdb ff = (get >>= (ff  . payments))

bdb :: MonadState DB m => (BalanceDB -> m a) -> m (a)
bdb ff = (get >>= (ff  . balances))

newIPNU :: IPNMessage -> Update DB ()
newIPNU msg =  mdb $ \ db -> put (IxSet.insert msg db)

getAccount :: Origin -> Destination -> UpdateDB (Balance)
getAccount orgn dest = bdb $ \ db -> do 
    do
            return $ getOne $ db @* [orgn] @* [dest]
    `justOr` do
            let new = (Balance origin dest (Cents 0) minBound)
            put $ IxSet.insert new
            return $ new

distributeP :: Payment -> Update DB (Either String ())
distributeP pp = runErrorT $ do
    pdb $ \ db -> (getOne db @* [(txn_id pp)]) `nothingOr` (throwError "duplicate payment")
    withPDB $ \ db -> put (IxSet.insert payment db)
    acc <- getAccount (Origin $ payer_email pp) (Destination $ reciever_email pp)
    let updated = acc { cents = (cents acc) + (payment_gross pp) - (payment_free pp) 
                      , time = (time acc) `max` (payment_date pp) }
    bdb $ \ db -> put $ IxSet.insert updated db

distributeValidatedPayment :: IPNMessage -> Payment -> Update DB (Either String ())
distributeValidatedPayment msg payment = do
    removeIPN msg
    distributeP payment

currentBalance :: Email -> Update DB Cents 
currentBalance email = do
    dests <- bdb $ db @* [(map Destination email)] 
    let 
            fromDest (Destination dst) = dst
            origins = map (Origin . fromDest) dests
    origins <- bdb $ db @* [(map Origin emails)]
    return $ (sum $ map cents dests) - (sum $ map cents origins)

