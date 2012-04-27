{-# LANGUAGE FlexibleContexts, FlexibleInstances, DeriveDataTypeable, TemplateHaskell #-}
module Data.DB where

import Control.Monad.State                      ( get, put, MonadState )
import Control.Monad.Reader                     ( ask, MonadReader )
import Data.Data                                ( Data, Typeable )
import qualified Data.Login                     as L
import qualified Data.CharityInfo               as C
import qualified Data.UserInfo                  as U
import qualified Data.Paypal                    as P
import Data.SafeCopy

data DB = DB { logins :: L.LoginDB 
             , charities :: C.CharityInfoDB 
             , userInfos :: U.UserInfoDB
             , ipnMessages :: P.IPNMessageDB 
             , payments :: P.PaymentDB 
             , deposits :: P.DepositDB 
             }
        deriving (Eq, Ord, Show, Data, Typeable)


$(deriveSafeCopy 0 'base ''DB)

getU :: MonadState DB m => m U.UserInfoDB
getU = get >>= return . userInfos

askM :: MonadReader DB m => m P.IPNMessageDB
askM = ask >>= return . ipnMessages

getM :: MonadState DB m => m P.IPNMessageDB
getM = get >>= return . ipnMessages

putM :: MonadState DB m => P.IPNMessageDB -> m ()
putM vv = get >>= (\ db -> put  db { ipnMessages = vv })

askP :: MonadReader DB m => m P.PaymentDB
askP = ask >>= return . payments

getP :: MonadState DB m => m P.PaymentDB
getP = get >>= return . payments

putP :: MonadState DB m => P.PaymentDB -> m ()
putP vv = get >>= (\ db -> put  db { payments = vv })

askD :: MonadReader DB m => m P.DepositDB
askD = ask >>= return . deposits

getD :: MonadState DB m => m P.DepositDB
getD = get >>= return . deposits

putD :: MonadState DB m => P.DepositDB -> m ()
putD vv = get >>= (\ db -> put  db { deposits = vv })
