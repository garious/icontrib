{-# LANGUAGE OverloadedStrings #-}

module UserInfoTest where

import Data.Acid.Memory                      ( openMemoryState )
import qualified UserInfo                   as U
import qualified Data.UserInfo              as U

import TestUtil

lookupTest :: IO ()
lookupTest = do
    db <- openMemoryState U.empty
    assertEqErrorT "lookup empty"  (U.lookupByOwner db "anatoly")   (Left "DoesntExist")

updateInfoTest :: IO ()
updateInfoTest = do
    db <- openMemoryState U.empty
    let ui :: U.UserInfo
        ui = U.UserInfo "anatoly" "first" "last" "phone" "email" "imageurl" 100 100 [] [] []
    assertEqErrorT "update" (U.updateInfo db "anatoly" ui) (Right ())
    assertEqErrorT "updated"  (U.lookupByOwner db "anatoly")   (Right ui)
    assertEqM "list"  (U.list db )   ["anatoly"]

mostInfluentialTest :: IO ()
mostInfluentialTest = do
    db <- openMemoryState U.empty
    assertEqErrorT "mostInfluential empty"  (U.mostInfluential db) (Left "DoesntExist")
    let toly :: U.UserInfo
        toly = U.UserInfo "anatoly" "first" "last" "phone" "email" "imageurl" 100 100 [] [] []
    assertEqErrorT "update" (U.updateInfo db "anatoly" toly) (Right ())
    let greg = U.UserInfo "greg" "greg" "fitz" "phone" "email" "foo" 200 100 [] [] []
    assertEqErrorT "update" (U.updateInfo db "greg" greg) (Right ())
    assertEqErrorT "updated"  (U.mostInfluential db)  (Right "greg")

main :: IO ()
main = do
    lookupTest
    updateInfoTest 
    mostInfluentialTest 
