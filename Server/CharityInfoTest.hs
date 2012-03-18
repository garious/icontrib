{-# LANGUAGE OverloadedStrings #-}

module CharityInfoTest where

import qualified CharityInfo                 as C
import qualified Data.CharityInfo            as C
import qualified DB                          as DB
import qualified Data.Login                  as L

import TestUtil

ident :: L.Identity
ident = (L.Identity "anatoly")

lookupTest :: IO ()
lookupTest = do
    db <- DB.emptyMemoryDB
    assertEqM "lookup empty"  (C.queryByOwner db ident)   []

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
    assertEqM "update" (C.updateInfo db ident ci) ()
    assertEqM "updated" (C.queryByOwner db ident)   ([ci])
    assertEqM "update2" (C.updateInfo db ident ci) ()

main :: IO ()
main = do
    lookupTest
    updateInfoTest 
