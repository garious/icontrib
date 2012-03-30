{-# LANGUAGE TypeFamilies, TemplateHaskell, DeriveDataTypeable, FlexibleContexts #-}
module Query.Login where

import Control.Monad.State                   ( get, put, MonadState )
import Control.Monad.Reader                  ( ask, MonadReader )
import Query.UserInfo                        ( userInfoU )
import qualified Data.UserInfo               as U
import qualified Data.IxSet                  as IxSet
import Data.IxSet                            ( (@*) )
import qualified Crypto.Hash.SHA512          as SHA512
import qualified Data.ByteString.Lazy        as BL
import qualified Data.ByteString             as BS
import qualified Log                         as Log
import Data.Acid
import SiteError
import Data.Login
import Data.DB
replace :: MonadState DB m => (LoginDB -> m LoginDB) -> m ()
replace ff = do
    db <- get
    ll <- ff (logins db)
    db' <- get
    put $ db' { logins = ll }

use :: MonadReader DB m => (LoginDB -> m b) -> m b
use ff = do
    db <- ask
    ff (logins db)

addIdentityU :: Identity -> PasswordHash -> Update DB (Either String ())
addIdentityU uid@(Identity email) phash = runErrorT $ replace $ \ db -> do
    let notExist mm = mm `nothingOr` alreadyExists
    notExist $ IxSet.getOne $ db @* [uid]
    lift $ userInfoU $ U.empty { U.owner = uid, U.email = email }
    return $ IxSet.insert (Login uid phash []) db

tokenToIdentityQ :: Token -> Query DB (Either String Identity)
tokenToIdentityQ token = runErrorT $ use $ \ db -> do
    liftM ident $ (IxSet.getOne $ db @* [token]) `justOr` badToken

listIdentitiesQ :: Query DB [Identity]
listIdentitiesQ = use $ \ db -> do
    return $ map ident $ IxSet.toList $ db

addIdentityTokenU :: Identity -> Token -> Update DB ()
addIdentityTokenU uid tok = runErrorT_ $ replace $ \ db -> do
    ll@(Login _ phash tts) <- (IxSet.getOne $ db @* [uid]) `justOr` doesntExist 
    return $ IxSet.insert (Login uid phash (tok:tts)) $ IxSet.delete ll db

clearIdentityTokensU :: Identity -> Update DB ()
clearIdentityTokensU uid = runErrorT_ $ replace $ \ db -> do
    ll@(Login _ phash _) <- (IxSet.getOne $ db @* [uid]) `justOr` doesntExist 
    return $ IxSet.insert (Login uid phash []) $ IxSet.delete ll db

checkPasswordQ :: Identity -> Password -> Query DB (Either String ())
checkPasswordQ uid pwd = runErrorT $ use $ \ db -> do
    (Login _ (PasswordHash phash salt) _) <- (IxSet.getOne $ db @* [uid]) `justOr` doesntExist 
    if phash == hashPassword salt pwd then return () else badPassword

hashPassword :: Salt -> Password -> Hash
hashPassword salt pwd = (iterate step pwdS) !! iterationCount
    where iterationCount = 100
          pwdS = toS pwd
          step chain = SHA512.hash (chain `BS.append` salt)

toS :: BL.ByteString -> BS.ByteString
toS = BS.concat . BL.toChunks


