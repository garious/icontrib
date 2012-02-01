module UserInfoTest where

import Data.Acid.Memory                      ( openMemoryState )
import qualified UserInfo                   as U
import qualified Data.UserInfo              as U

import TestUtil
import ServerError

lookupTest :: IO ()
lookupTest = do
    db <- openMemoryState U.empty
    assertEqErrorT "lookup empty"  (U.lookupInfo db (toB "anatoly"))   (Left UserDoesntExist)

updateInfoTest :: IO ()
updateInfoTest = do
    db <- openMemoryState U.empty
    let ui = U.UserInfo (toB "anatoly") "anatoly" "yako" "foo" 100 100 [] []
    U.updateInfo db (toB "anatoly") ui
    assertEqErrorT "updated"  (U.lookupInfo db (toB "anatoly"))   (Right ui)
    assertEqM "list"  (U.list db )   [(toB "anatoly")]

mostInfluentialTest :: IO ()
mostInfluentialTest = do
    db <- openMemoryState U.empty
    assertEqErrorT "mostInfluential empty"  (U.mostInfluential db) (Left UserDoesntExist)
    let toly = U.UserInfo (toB "anatoly") "anatoly" "yako" "foo" 100 100 [] []
    U.updateInfo db (toB "anatoly") toly
    let greg = U.UserInfo (toB "greg") "greg" "fitz" "foo" 200 100 [] []
    U.updateInfo db (toB "greg") greg
    assertEqErrorT "updated"  (U.mostInfluential db)  (Right $ toB "greg")

main :: IO ()
main = do
    lookupTest
    updateInfoTest 
    mostInfluentialTest 
