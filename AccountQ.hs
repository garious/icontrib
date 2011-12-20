module AccountQ where

import Account
import ServerError
import Char                                  ( ord )
import Data.Acid.Memory                      ( openMemoryState )
import qualified Data.ByteString.Lazy             as B

toB :: String -> B.ByteString
toB ss = B.pack $ map (fromIntegral . ord) ss

test :: IO ()
test = do
   let assert msg False = error msg
       assert _ True = return $ ()
       isRight (Right _)   = True
       isRight _           = False

   db <- openMemoryState empty

   _rv <- listUsers db
   assert "listUsers" (null _rv)

   _rv <- loginToCookie db (toB "hello") (toB "world")
   assert "login empty" ((Left UserDoesntExist) == _rv)

   _rv <- cookieToUser db (toB "randomnonsense")
   assert "cookieToUser empty" ((Left BadCookie) == _rv)

   _rv <- addUser db (toB "hello") (toB "world")
   assert "added user" (_rv == (Right ()))

   _rv <- listUsers db
   assert "listUsers2" ([toB "hello"] == _rv)

   _rv <- addUser db (toB "hello") (toB "again")
   assert "added user" (_rv == (Left UserAlreadyExists))

   _rv <- loginToCookie db (toB "hello") (toB "badpassword")
   assert "login bad password" (_rv == (Left BadPassword))

   _rv <- loginToCookie db (toB "hello") (toB "world")
   assert "loginToCookie good password" (isRight _rv)
   let (Right goodcookie) = _rv

   _rv <- cookieToUser db (toB "randomnonsense") 
   assert "cookieToUser bad cookie" (_rv == (Left BadCookie))

   _rv <- cookieToUser db goodcookie
   assert "cookieToUser good cookie" (_rv == (Right (toB "hello")))

   clearUserCookie db (toB "hello")
   _rv <- cookieToUser db goodcookie
   assert "cookieToUser good cookie" (_rv == (Left BadCookie))
