{-# LANGUAGE FlexibleContexts #-}
module DB.Paypal

import Data.CharityInfo
import Data.Login                            ( Identity )
import Data.Acid                             ( query, update )
import SiteError
import Data.Popular                          ( Popular(Popular) )
import Query.DB
import qualified Data.IxSet                  as IxSet

clearValidatedPayment :: (MonadError String m, MonadIO m) => Database -> Payment -> m () 
clearValidatedPayment db payment = throwLeft $ update db (ClearValidatedPaymentU payment)

