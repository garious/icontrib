module ServerMain where

import Server
import qualified Account as Account
import qualified Data.ByteString.Lazy        as BL
import Data.Acid.Memory                      ( openMemoryState )
import Happstack.Lite

main :: IO ()
main = do
   db <- openMemoryState Account.empty
   index <- BL.readFile "index.html"
   let 
         homePage :: ServerPart Response
         homePage = ok $ toResponse index
   serve Nothing ( msum [ dir "addUser" (addUser db)
                        , homePage
                        ])

