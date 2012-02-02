{-# LANGUAGE OverloadedStrings #-}

module UserInfoTest where

import Data.Acid.Memory                      ( openMemoryState )
import qualified CharityInfo                 as C

import TestUtil
import ServerError

lookupTest :: IO ()
lookupTest = do
    db <- openMemoryState C.empty
    assertEqErrorT "lookup empty"  (C.lookupInfo db "anatoly")   (Left UserDoesntExist)

updateInfoTest :: IO ()
updateInfoTest = do
    db <- openMemoryState C.empty
    let ci = C.CharityInfo (C.OrganizationInfo "ein" "name" "website") (C.PointOfContact "first" "last" "phone" "email")
    C.updateInfo db "anatoly" ci
    assertEqErrorT "updated"  (C.lookupInfo db "anatoly")   (Right ci)

main :: IO ()
main = do
    lookupTest
    updateInfoTest 
