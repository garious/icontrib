{-# LANGUAGE FlexibleContexts #-}
module DB.Paypal where

import Data.Acid                             ( update )
import Data.Paypal                           ( Payment, IPNMessage, ProductID(..) )
import DB.Login                              (randomWordIO)
import qualified Codec.Binary.Url            as Url
import SiteError
import Query.DB

clearValidatedPayment :: (MonadError String m, MonadIO m) => Database -> IPNMessage -> Payment -> m () 
clearValidatedPayment db msg payment = throwLeft $ update db (ClearValidatedPaymentU msg payment)

newProductID :: IO ProductID
newProductID = do
    pid <- liftM (take 255 . Url.encode) $ sequence $ replicate 255 randomWordIO
    return $ ProductID pid
