{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Data.Login where

import Data.Data                             ( Data, Typeable )
import Data.IxSet
import Data.SafeCopy
import qualified Data.ByteString.Lazy        as BL
import qualified Data.ByteString             as BS

newtype Identity  = Identity BL.ByteString
                  deriving (Eq, Ord, Show, Data, Typeable)
newtype Token     = Token BL.ByteString
                  deriving (Eq, Ord, Show, Data, Typeable)
type Password     = BL.ByteString

type Hash      = BS.ByteString
type Salt      = BS.ByteString

$(deriveSafeCopy 0 'base ''Identity)
$(deriveSafeCopy 0 'base ''Token)

data PasswordHash = PasswordHash Hash Salt
                  deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''PasswordHash)


data Login = Login { ident :: Identity
                   , password :: PasswordHash
                   , tokens :: [Token]
                   }
                 deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Login)

instance Indexable Login where
    empty = ixSet [ ixFun $ \ci -> [ ident ci ]
                  , ixFun $ \ci -> tokens ci
                  ]

type LoginDB = IxSet Login
