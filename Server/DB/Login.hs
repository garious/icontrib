{-# LANGUAGE FlexibleContexts #-}
module DB.Login where
import Data.Char                             ( ord )
import Control.Monad                         ( when )
import System.Random                         ( randomRIO )
import Data.Word                             ( Word8 )

import qualified Data.ByteString.Lazy        as BL
import qualified Data.ByteString             as BS

import SiteError
import Query.DB
import Query.Login                           ( hashPassword )
import Data.Login
import Data.Acid

toIdentity :: String -> Identity
toIdentity str = Identity $ toBL str

toToken :: String -> Token
toToken str = Token $ toBL str

tokenUnpack :: Token -> [Word8]
tokenUnpack (Token str) = BL.unpack str

toPassword :: String -> Password
toPassword str = toBL str

toBL :: String -> BL.ByteString
toBL str = BL.pack (map (fromIntegral . ord) str)

listIdentities :: Database ->  IO [Identity]
listIdentities db = query db ListIdentitiesQ

addIdentity :: (MonadError String m, MonadIO m) => Database -> Identity -> Password -> m ()
addIdentity db uid@(Identity uidstr) pwd = do
   salt <- liftIO $ newSalt
   when (BL.null uidstr) (badUsername)
   when (BL.null pwd) (badPassword)
   throwLeft $ update db (AddIdentityU uid (PasswordHash (hashPassword salt pwd) salt))

checkPassword :: (MonadError String m, MonadIO m) => Database -> Identity -> Password -> m ()
checkPassword db uid pwd = throwLeft $ query db (CheckPasswordQ uid pwd)
  
loginToToken :: (MonadError String m, MonadIO m) => Database -> Identity -> Password -> m Token
loginToToken db uid pwd = do
   checkPassword db uid pwd
   token <- liftM Token $ liftIO $ newToken
   _ <- liftIO $ update db (AddIdentityTokenU uid token)
   return token

tokenToIdentity :: (MonadError String m, MonadIO m) => Database -> Token -> m Identity
tokenToIdentity db token = throwLeft $ query db (TokenToIdentityQ token)

clearIdentityTokens :: Database -> Identity -> IO ()
clearIdentityTokens db uid = update db (ClearIdentityTokensU uid)

randomWordIO :: IO Word8
randomWordIO = liftM fromIntegral $ randomRIO ((fromIntegral (minBound :: Word8))::Int, (fromIntegral (maxBound :: Word8))::Int)

newSalt :: IO BS.ByteString
newSalt = liftM BS.pack $ sequence $ take 64 $ repeat randomWordIO

newToken :: IO BL.ByteString
newToken = liftM BL.pack $ sequence $ take 64 $ repeat randomWordIO


