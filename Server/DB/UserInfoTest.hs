{-# LANGUAGE OverloadedStrings #-}

module UserInfoTest where

import qualified DB.DB                      as DB
import qualified DB.UserInfo                as U
import qualified Data.UserInfo              as U
import qualified Data.Login                 as L

import TestUtil

toly :: L.Identity
toly = (L.Identity "anatoly")

tolyi :: U.UserInfo
tolyi = U.UserInfo toly "first" "last" "phone" "email" "imageurl" 100 100 [] [] []

greg :: L.Identity
greg = (L.Identity "greg")

gregi :: U.UserInfo
gregi = U.UserInfo greg "greg" "fitz" "phone" "email" "foo" 200 100 [] [] []

lookupTest :: IO ()
lookupTest = do
    db <- DB.emptyMemoryDB
    assertEqErrorT "lookup empty"  (U.queryByOwner db toly)   (Left "DoesntExist")

updateInfoTest :: IO ()
updateInfoTest = do
    db <- DB.emptyMemoryDB
    assertEqErrorT "update"     (U.updateInfo db toly tolyi)   (Right ())
    assertEqErrorT "updated"    (U.queryByOwner db toly)       (Right tolyi)
    assertEqM       "list"      (U.list db )                   [toly]

mostInfluentialTest :: IO ()
mostInfluentialTest = do
    db <- DB.emptyMemoryDB
    assertEqErrorT "mostInfluential empty"  (U.mostInfluential db) (Left "DoesntExist")
    assertEqErrorT "update" (U.updateInfo db toly tolyi) (Right ())
    assertEqErrorT "update" (U.updateInfo db greg gregi) (Right ())
    assertEqErrorT "updated"  (U.mostInfluential db)  (Right greg)

main :: IO ()
main = do
    lookupTest
    updateInfoTest 
    mostInfluentialTest 
