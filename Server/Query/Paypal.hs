module Query.Paypal where

withMDB :: MonadState DB m => (IPNMessageDB -> m IPNMessageDB) -> m ()
withMDB ff = (get >>= (ff  . ipnMessages))

withPDB :: MonadState DB m => (PaymentDB -> m a) -> m (a)
withPDB ff = (get >>= (ff  . payments))

withBDB :: MonadState DB m => (BalanceDB -> m a) -> m (a)
withBDB ff = (get >>= (ff  . balances))

newIPNU :: IPNMessage -> Update DB ()
newIPNU msg =  updateMDB $ \ db -> put (IxSet.insert msg db)

removeIPN :: IPNMessage -> Update DB ()
removeIPN msg = updateMDB $ \ db -> put (IxSet.delete msg db)

getAccount :: Origin -> Destination -> UpdateDB (Balance)
getAccount orgn dest = updateBDB $ \ db -> do 
    do
            return $ getOne $ db @* [orgn] @* [dest]
    `justOr` do
            let new = (Balance origin dest (Cents 0) minBound)
            put $ IxSet.insert new
            return $ new

distributeP :: Payment -> Update DB ()
distributeP pp = runErrorT $ do
    withPDB $ \ db -> (getOne db @* [(txn_id pp)]) `nothingOr` (throwError "duplicate payment")
    withPDB $ \ db -> put (IxSet.insert payment db)
    acc <- getAccount (Origin $ payer_email pp) (Destination $ reciever_email pp)
    let updated = acc { cents = (cents acc) + (payment_gross pp) - (payment_free pp) 
                      , time = (time acc) `max` (payment_date pp) }
    withBDB $ \ db -> put $ IxSet.insert updated db

distributeValidatedPayment :: IPNMessage -> Payment -> Update DB ()
distributeValidatedPayment msg payment = do
    removeIPN msg
    distributeP payment

