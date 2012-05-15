{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Data.Login where

import Data.Data                             ( Data, Typeable )
import Data.IxSet
import Data.SafeCopy
import Data.Char                             ( toUpper ) 
import qualified Data.ByteString.Lazy        as BL
import qualified Data.ByteString             as BS

newtype Identity  = Identity String
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

upcase :: Identity -> Identity 
upcase (Identity uu) = Identity $ map toUpper uu

data Login = Login { ident :: Identity
                   , password :: PasswordHash
                   , tokens :: [Token]
                   }
                 deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Login)


instance Indexable Login where
    empty = ixSet [ ixFun $ \ci -> [ upcase $ ident ci ]
                  , ixFun $ \ci -> tokens ci
                  ]

type LoginDB = IxSet Login
