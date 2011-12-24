import Site
import qualified Account as Account
import Data.Acid.Memory                      ( openMemoryState )
import Happstack.Lite

main :: IO ()
main = do
   db <- openMemoryState Account.empty
   let 
         homePage :: ServerPart Response
         homePage = serveDirectory DisableBrowsing ["index.html"] "public"
         
   serve Nothing ( msum [ dir "get_user" (getUser db)
                        , homePage
                        ])

