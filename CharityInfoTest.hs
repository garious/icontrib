{-# LANGUAGE OverloadedStrings #-}

module UserInfoTest where

import Data.Acid.Memory                      ( openMemoryState )
import qualified CharityInfo                 as C
import qualified Data.CharityInfo            as C

import TestUtil

lookupTest :: IO ()
lookupTest = do
    db <- openMemoryState C.empty
    assertEqM "lookup empty"  (C.lookupByOwner db "anatoly")   []

updateInfoTest :: IO ()
updateInfoTest = do
    db <- openMemoryState C.empty
    let ci = C.CharityInfo "anatoly"
                           "firstname" 
                           "lastname" 
                           "phone"
                           "email"
                           (C.Ein "ein") 
                           "name"
                           "website"
                           "mission"
                           (C.CharityID "cid")
                           "imageurl"
    assertEqErrorT "update" (C.updateInfo db "anatoly" ci) (Right ())
    assertEqM "updated" (C.lookupByOwner db "anatoly")   ([ci])

main :: IO ()
main = do
    lookupTest
    updateInfoTest 
