{-# LANGUAGE TypeFamilies, TemplateHaskell, DeriveDataTypeable #-}
module DB( Database
         , emptyDB
         , newFromFile
         , emptyMemoryDB
         ) where

import Data.DB
import qualified Data.IxSet                  as IxSet
import Data.Acid.Memory                      ( openMemoryState )
import Data.Acid
import Query.DB                              ( Database )


emptyDB :: DB
emptyDB = DB IxSet.empty IxSet.empty IxSet.empty

newFromFile :: FilePath -> IO Database
newFromFile fp = openLocalStateFrom fp emptyDB

emptyMemoryDB :: IO Database
emptyMemoryDB = openMemoryState emptyDB

