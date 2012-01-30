module UserInfoTest where

import Data.Acid.Memory                      ( openMemoryState )
import qualified UserInfo                    as U
import qualified Text.JSON                   as JS

import TestUtil
import ServerError

lookupTest :: IO ()
lookupTest = do
    db <- openMemoryState U.empty
    assertEqErrorT "lookup empty"  (U.lookupInfo db (toB "anatoly"))   (Left UserDoesntExist)

updateInfoTest :: IO ()
updateInfoTest = do
    db <- openMemoryState U.empty
    let ui = U.UserInfo "anatoly" "yako" "foo" 100 100 [] []
    putStrLn (JS.encode ui)
    U.updateInfo db (toB "anatoly") ui
    assertEqErrorT "updated"  (U.lookupInfo db (toB "anatoly"))   (Right ui)

mostInfluentialTest :: IO ()
mostInfluentialTest = do
    db <- openMemoryState U.empty
    assertEqErrorT "mostInfluential empty"  (U.mostInfluential db) (Left UserDoesntExist)
    let toly = U.UserInfo "anatoly" "yako" "foo" 100 100 [] []
    U.updateInfo db (toB "anatoly") toly
    let greg = U.UserInfo "greg" "fitz" "foo" 200 100 [] []
    U.updateInfo db (toB "anatoly") greg
    assertEqErrorT "updated"  (U.mostInfluential db)  (Right greg)

main :: IO ()
main = do
    lookupTest
    updateInfoTest 
    mostInfluentialTest 
