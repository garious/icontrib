{-# LANGUAGE OverloadedStrings #-}

module CharityInfoTest where

import qualified DB.CharityInfo              as C
import qualified Data.CharityInfo            as C
import qualified DB.DB                       as DB
import qualified Data.Login                  as L
import qualified Data.IxSet                  as IxSet
import TestUtil

ident :: L.Identity
ident = (L.Identity "anatoly")

lookupTest :: IO ()
lookupTest = do
    db <- DB.emptyMemoryDB
    assertEqM "lookup empty"  (C.queryByOwner db ident)   IxSet.empty 

updateInfoTest :: IO ()
updateInfoTest = do
    db <- DB.emptyMemoryDB
    let ci = C.CharityInfo ident
                           (C.Ein "ein") 
                           "name"
                           "website"
                           (C.CharityID "cid")
                           "imageurl"
                           "mission"
                           "paymentaddress"

    assertEqErrorT "update"     (C.updateInfo db ident ci) (Right ())
    assertMEq      "updated" [ci] $ do
        rvs <- C.queryByOwner db ident
        return $ IxSet.toList rvs 
    assertEqErrorT "update2"    (C.updateInfo db ident ci) (Right ())

main :: IO ()
main = do
    lookupTest
    updateInfoTest 
