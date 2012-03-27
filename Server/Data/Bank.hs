module Data.Bank where

import qualified Data.Login                  as L
import qualified Data.CharityInfo            as C

data Ledger = User L.Identity Cents
            | Charity C.Ein Cents
