module ServerQ where

--import Server
--import qualified Account as Account
--import Data.Acid.Memory                      ( openMemoryState )
import Happstack.Lite

test :: IO ()
test = do
   --db <- openMemoryState Account.empty
   let 
         homePage :: ServerPart Response
         homePage = serveDirectory DisableBrowsing ["index.html"] "frontend/public"
         
   serve Nothing ( msum [ -- dir "addUser" (addUser db)
                        homePage
                        ])

