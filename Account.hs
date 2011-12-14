{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Account( addUser
              , loginToCookie 
              , cookieToUser
              , empty
              , test
              )  where

import Data.Word                             ( Word8 )
import Char                                  ( ord )
import Monad                                 ( liftM )
import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )
import Control.Applicative                   ( (<$>) )
import System.Environment                    ( getArgs )
import Data.Maybe                            ( isJust, fromJust, isNothing )
import Data.Acid.Memory                      ( openMemoryState )
import qualified Data.Map                    as Map
import qualified Crypto.Hash.SHA512          as SHA512
import qualified Data.ByteString             as B
import Data.Acid
import Data.SafeCopy
import Random(randomIO, Random, random, randomR)

type UserID    = B.ByteString
type Password  = B.ByteString
type Cookie    = B.ByteString

type Hash      = B.ByteString
type Salt      = B.ByteString

data PasswordHash = PasswordHash Hash Salt
$(deriveSafeCopy 0 'base ''PasswordHash)

data UserInfo = UserInfo PasswordHash
$(deriveSafeCopy 0 'base ''UserInfo)


data Database = Database { users :: (Map.Map UserID UserInfo) 
                         , cookies :: (Map.Map Cookie UserID)
                         }
$(deriveSafeCopy 0 'base ''Database)

empty :: Database
empty = Database Map.empty Map.empty

addUserU :: UserID -> PasswordHash -> Update Database ()
addUserU uid phash = do
   db <- get
   put $ db { users = (Map.insert uid (UserInfo phash) (users db)) }

cookieToUserQ :: Cookie -> Query Database (Maybe UserID)
cookieToUserQ cookie = do
   db <- ask
   return $ Map.lookup cookie (cookies db)

addUserCookieU :: UserID -> Cookie -> Update Database ()
addUserCookieU uid cookie = do
   db <- get
   put $ db { cookies = (Map.insert cookie uid (cookies db)) }

checkPasswordQ :: UserID -> Password -> Query Database Bool
checkPasswordQ uid pwd = do
   db <- ask
   let checkPass Nothing = False
       checkPass (Just (UserInfo (PasswordHash hash salt))) = hash == hashPassword salt pwd 
   return $ checkPass (Map.lookup uid (users db))

hashPassword :: Salt -> Password -> Hash
hashPassword salt pwd = (iterate step pwd) !! iterationCount
    where iterationCount = 100
          step chain = SHA512.hash (chain `B.append` salt)

$(makeAcidic ''Database ['addUserU, 'checkPasswordQ, 'addUserCookieU, 'cookieToUserQ])

addUser :: AcidState Database -> UserID -> Password -> IO ()
addUser db uid pwd = do
   salt <- newSalt
   update db (AddUserU uid (PasswordHash salt (hashPassword salt pwd)))

loginToCookie :: AcidState Database -> UserID -> Password -> IO (Maybe Cookie) 
loginToCookie db uid pwd = do
   check <- query db (CheckPasswordQ uid pwd) 
   case(check) of
      False -> return Nothing
      True -> do
         cookie <- newCookie
         update db (AddUserCookieU uid cookie)
         return $ Just cookie

cookieToUser :: AcidState Database -> Cookie -> IO (Maybe UserID)
cookieToUser db cookie = query db (CookieToUserQ cookie)

instance Random Word8 where
    randomR (lo, hi) rng = let (val, rng') = randomR (fromIntegral lo, fromIntegral hi) rng
                               val :: Int
                           in  (fromIntegral val, rng')
    random rng = randomR (minBound, maxBound) rng
 
newSalt :: IO B.ByteString
newSalt = liftM B.pack $ sequence $ take 32 $ repeat randomIO

newCookie :: IO B.ByteString
newCookie = newSalt

test :: IO ()
test = do
   let assert msg False = error msg
       assert _ True = return $ ()
       toB :: String -> B.ByteString
       toB ss = B.pack $ map (fromIntegral . ord) ss

   db <- openMemoryState empty

   rv <- loginToCookie db (toB "hello") (toB "world")
   assert "login empty" (isNothing rv)

   rv <- cookieToUser db (toB "randomnonsense")
   assert "cookieToUser empty" (isNothing rv)

   addUser db (toB "hello") (toB "world")
   rv <- loginToCookie db (toB "hello") (toB "badpassword")
   assert "login bad password" (isNothing rv)

   rv <- loginToCookie db (toB "hello") (toB "world")
   print $ rv
   assert "loginToCookie good password" (isJust rv)

   rv <- cookieToUser db (toB "randomnonsense") 
   assert "cookieToUser bad cookie" (isNothing rv)

   rv <- cookieToUser db (fromJust rv)
   assert "cookieToUser good cookie" (rv == (Just (toB "hello")))
