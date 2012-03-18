module Login where
import Data.Char                             ( ord )
import Control.Monad.IO.Class                ( MonadIO, liftIO )
import Control.Monad                         ( when, liftM )
import ServerError                           ( badUsername, badPassword, failLeftIO )
import Random                                ( randomIO, Random, random, randomR )
import Data.Word                             ( Word8 )

import qualified Data.ByteString.Lazy        as BL
import qualified Data.ByteString             as BS

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

addIdentity :: MonadIO m => Database -> Identity -> Password -> m ()
addIdentity db uid@(Identity uidstr) pwd = do
   salt <- liftIO $ newSalt
   when (BL.null uidstr) (badUsername)
   when (BL.null pwd) (badPassword)
   failLeftIO $ update db (AddIdentityU uid (PasswordHash (hashPassword salt pwd) salt))

checkPassword :: MonadIO m => Database -> Identity -> Password -> m ()
checkPassword db uid pwd = failLeftIO $ query db (CheckPasswordQ uid pwd)
  
loginToToken :: MonadIO m => Database -> Identity -> Password -> m Token
loginToToken db uid pwd = do
   checkPassword db uid pwd
   token <- liftM Token $ liftIO $ newToken
   _ <- liftIO $ update db (AddIdentityTokenU uid token)
   return token

tokenToIdentity :: MonadIO m => Database -> Token -> m Identity
tokenToIdentity db token = failLeftIO $ query db (TokenToIdentityQ token)

clearIdentityTokens :: Database -> Identity -> IO ()
clearIdentityTokens db uid = update db (ClearIdentityTokensU uid)

instance Random Word8 where
    randomR (lo, hi) rng = let (val, rng') = randomR (fromIntegral lo, fromIntegral hi) rng
                               val :: Int
                           in  (fromIntegral val, rng')
    random rng = randomR (minBound, maxBound) rng
 
newSalt :: IO BS.ByteString
newSalt = liftM BS.pack $ sequence $ take 64 $ repeat randomIO

newToken :: IO BL.ByteString
newToken = liftM BL.pack $ sequence $ take 64 $ repeat randomIO


