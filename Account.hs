{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fspec-constr-count=2 -fno-warn-orphans #-}
module Account( addUser
              , loginToCookie 
              , cookieToUser
              , clearUserCookie 
              , empty
              , test
              )  where
import Control.Monad.IO.Class                ( MonadIO )
import Data.Word                             ( Word8 )
import Char                                  ( ord )
import Monad                                 ( liftM )
import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )
import Data.Acid.Memory                      ( openMemoryState )
import Control.Monad.Error                   ( Error, runErrorT, throwError, MonadError, liftIO)
import Data.Typeable                         ()
import qualified Data.Map                    as Map
import qualified Crypto.Hash.SHA512          as SHA512
import qualified Data.ByteString             as B
import Data.Acid
import Data.SafeCopy
import Data.Data
import Random(randomIO, Random, random, randomR)

type UserID    = B.ByteString
type Password  = B.ByteString
type Cookie    = B.ByteString

type Hash      = B.ByteString
type Salt      = B.ByteString

data PasswordHash = PasswordHash Hash Salt
$(deriveSafeCopy 0 'base ''PasswordHash)

data AccountError = UserAlreadyExists
                  | UserDoesntExist
                  | BadPassword
                  | BadCookie
                  deriving (Data, Typeable, Eq, Show)
$(deriveSafeCopy 0 'base ''AccountError)

instance Error AccountError

data Database = Database { users :: (Map.Map UserID PasswordHash) 
                         , userCookies :: (Map.Map UserID Cookie)
                         , cookies :: (Map.Map Cookie UserID)
                         }
$(deriveSafeCopy 0 'base ''Database)

empty :: Database
empty = Database Map.empty Map.empty Map.empty

addUserU :: UserID -> PasswordHash -> Update Database (Either AccountError ())
addUserU uid phash = runErrorT $ do
   db <- get
   case(Map.lookup uid (users db)) of
      Nothing -> put $ db { users = (Map.insert uid phash (users db)) }
      (Just _) -> throwError UserAlreadyExists

maybeE :: MonadError e m => e -> Maybe a -> m a
maybeE _ (Just a) = return a
maybeE ee _       = throwError ee

cookieToUserQ :: Cookie -> Query Database (Either AccountError UserID)
cookieToUserQ cookie = runErrorT $ do
   db <- ask
   maybeE BadCookie (Map.lookup cookie (cookies db))

listUsersQ :: Query Database [UserID]
listUsersQ = do
   db <- ask
   return $ map fst $ Map.toList (users db)

addUserCookieU :: UserID -> Cookie -> Update Database ()
addUserCookieU uid cookie = do
   db <- get
   put $ db { cookies = (Map.insert cookie uid (cookies db)) 
            , userCookies = (Map.insert uid cookie (userCookies db))
            }

clearUserCookieU :: UserID -> Update Database ()
clearUserCookieU uid = do
   db <- get
   case (Map.lookup uid (userCookies db)) of
      Nothing -> return ()
      (Just cc) -> put $ db { cookies = (Map.delete cc (cookies db))
                            , userCookies = (Map.delete uid (userCookies db))
                            }

checkPasswordQ :: UserID -> Password -> Query Database (Either AccountError ())
checkPasswordQ uid pwd = runErrorT $ do
   db <- ask
   let checkPass Nothing = throwError UserDoesntExist
       checkPass (Just (PasswordHash hash salt)) 
         | hash == hashPassword salt pwd = return $ ()
       checkPass _ = throwError BadPassword
   checkPass (Map.lookup uid (users db))

hashPassword :: Salt -> Password -> Hash
hashPassword salt pwd = (iterate step pwd) !! iterationCount
    where iterationCount = 100
          step chain = SHA512.hash (chain `B.append` salt)

$(makeAcidic ''Database ['addUserU, 'checkPasswordQ, 'addUserCookieU, 'cookieToUserQ, 'listUsersQ, 'clearUserCookieU ])

addUser :: AcidState Database -> UserID -> Password -> IO (Either AccountError ())
addUser db uid pwd = do
   salt <- newSalt
   update db (AddUserU uid (PasswordHash (hashPassword salt pwd) salt))

rethrowIO :: (MonadError e m, MonadIO m) => IO (Either e b) -> m b
rethrowIO aa = do
   err <- liftIO aa
   case(err) of
      (Left ee)   -> throwError ee
      (Right val) -> return val
   
loginToCookie :: AcidState Database -> UserID -> Password -> IO (Either AccountError Cookie) 
loginToCookie db uid pwd = runErrorT $ do
   _ <- rethrowIO $ query db (CheckPasswordQ uid pwd)
   cookie <- liftIO $ newCookie
   _ <- liftIO $ update db (AddUserCookieU uid cookie)
   return cookie

cookieToUser :: AcidState Database -> Cookie -> IO (Either AccountError UserID)
cookieToUser db cookie = query db (CookieToUserQ cookie)

clearUserCookie :: AcidState Database -> UserID -> IO ()
clearUserCookie db uid = update db (ClearUserCookieU uid)

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
       isRight (Right _)   = True
       isRight _           = False

   db <- openMemoryState empty

   _rv <- loginToCookie db (toB "hello") (toB "world")
   assert "login empty" ((Left UserDoesntExist) == _rv)

   _rv <- cookieToUser db (toB "randomnonsense")
   assert "cookieToUser empty" ((Left BadCookie) == _rv)

   _rv <- addUser db (toB "hello") (toB "world")
   assert "added user" (_rv == (Right ()))

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
