module ServerQ where

import Server
import qualified Account as Account
import Data.Acid.Memory                      ( openMemoryState )
import Happstack.Lite                        ( serve )

test :: IO ()
test = do
   db <- openMemoryState Account.empty
   serve Nothing (addUser db)
