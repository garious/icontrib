{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fspec-constr-count=2 -fno-warn-orphans #-}
module CharityInfo where

import Monad                                 ( liftM )
import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )
import Control.Monad.Error                   ( runErrorT, throwError, MonadError )
import Data.Typeable                         ()
import Data.Maybe                            ( fromMaybe )
import Data.List                             ( foldl' )
import Data.Typeable                         (Typeable)
import Control.Monad.IO.Class                ( MonadIO )
import qualified Text.JSON                   as JS
import qualified Data.Map                    as Map
import qualified Account                     as A

import Text.JSON
import Data.Derive.JSON
import Data.DeriveTH
import Data.Acid
import Data.SafeCopy
import ServerError

type CharityID    = A.UserID

data PointOfContact = PointOfContact { firstName :: String 
                                     , lastName :: String
                                     , phone :: String
                                     , email :: String
                                     }

$(deriveSafeCopy 0 'base ''PointOfContact)
$(derive makeJSON ''PointOfContact)

data CharityInfo = CharityInfo { ein :: String
                               , organizationName :: String
                               , companyWebSite :: String
                               , pointOfContact :: PointOfContact
                               }
                               deriving Typeable

$(deriveSafeCopy 0 'base ''CharityInfo)
$(derive makeJSON ''CharityInfo)

   
data Database = Database !(Map.Map CharityID CharityInfo)
$(deriveSafeCopy 0 'base ''Database)

empty :: Database
empty = Database Map.empty

--todo: do compare and exchange so you dont merge inside the db lock
mergeU :: CharityID -> String -> Update Database (Either ServerError ())
mergeU key str = runErrorT $ do
   jdata <- checkResult $ JS.decode $ str
   (Database db) <- get
   let oldval = (fromMaybe JS.JSNull) $ liftM JS.showJSON $ Map.lookup key db
   val <- checkResult $ merge oldval jdata
   put $ (Database $ Map.insert key val db)
   where
      merge :: JS.JSValue -> JS.JSValue -> JS.Result CharityInfo
      merge ov jd = JS.readJSON $ mergeJSON ov jd

checkResult :: MonadError ServerError m => Result a -> m a
checkResult (JS.Ok a)    = return a
checkResult (JS.Error _) = throwError RecordMergeError

mergeJSON :: JS.JSValue -> JS.JSValue -> JS.JSValue
mergeJSON (JS.JSObject old) (JS.JSObject new) = 
   JS.JSObject $ JS.toJSObject $ mergeList (JS.fromJSObject old) (JS.fromJSObject new)
   where
      replace kk vv ll = (kk,vv):(filter (((/=) kk) . fst) ll)
      mergeList oldlist newlist = foldl' mergeIntoList oldlist newlist
      mergeIntoList oldlist (key,newval) =
         case (lookup key oldlist) of
            (Nothing)      -> (key, newval):oldlist
            (Just oldval)  -> replace key (mergeJSON oldval newval) oldlist
mergeJSON _ new = new

lookupQ :: CharityID -> Query Database (Either ServerError CharityInfo)
lookupQ key = runErrorT $ do
   (Database db) <- ask
   checkMaybe UserDoesntExist $ Map.lookup key db


$(makeAcidic ''Database ['mergeU, 'lookupQ])

lookupInfo :: (MonadIO m, MonadError ServerError m) => AcidState Database -> CharityID -> m CharityInfo
lookupInfo db cid = A.rethrow $ query db (LookupQ cid)

updateInfo ::  (MonadIO m, MonadError ServerError m) => AcidState Database -> CharityID -> String -> m ()
updateInfo db cid str = A.rethrow $ update db (MergeU cid str)
