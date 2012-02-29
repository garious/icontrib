{-# LANGUAGE FlexibleContexts #-}
module JSONUtil where

import Data.Data                             ( Data )
import Control.Monad.Error                   ( MonadError, runErrorT )
import Control.Monad.Identity                ( runIdentity )
import Data.List                             ( foldl' )
import qualified Text.JSON.Generic           as JS
import qualified ServerError                 as SE
import qualified Text.JSON.String            as JSS


runJsonDecode :: Data b => String -> Either SE.ServerError b
runJsonDecode str = runIdentity $ runErrorT $ jsonDecode str

jsonDecodeE :: Data b => String -> b
jsonDecodeE str = JS.decodeJSON str

jsonDecode :: (MonadError SE.ServerError m, Data b) => String -> m b
jsonDecode str = do 
    val <- jsonParse str 
    checkJS $ JS.fromJSON $ val 

runJsonParse :: String -> Either SE.ServerError JS.JSValue
runJsonParse str = runIdentity $ runErrorT $ jsonParse str

jsonParse :: MonadError SE.ServerError m => String -> m JS.JSValue
jsonParse str = do
    let check (Left msg) = SE.jsonParseError msg
        check (Right j)  =  return j
    check $ JSS.runGetJSON JS.readJSValue str

jsonEncode :: Data a => a -> String 
jsonEncode = JS.encodeJSON

jsonUpdate :: (MonadError SE.ServerError m, Data b, Data a) => a -> String -> m b 
jsonUpdate val str = do
    jd <- jsonParse str
    let ov = JS.toJSON val
    checkJS $ JS.fromJSON $ jsonMerge ov jd

jsonMerge :: JS.JSValue -> JS.JSValue -> JS.JSValue
jsonMerge (JS.JSObject old) (JS.JSObject new) =
   JS.JSObject $ JS.toJSObject $ mergeList (JS.fromJSObject old) (JS.fromJSObject new)
   where
      replace kk vv ll = (kk,vv):(filter (((/=) kk) . fst) ll)
      mergeList oldlist newlist = foldl' mergeIntoList oldlist newlist
      mergeIntoList oldlist (key,newval) =
         case (lookup key oldlist) of
            (Nothing)      -> (key, newval):oldlist
            (Just oldval)  -> replace key (jsonMerge oldval newval) oldlist
jsonMerge _ new = new

checkJS :: MonadError SE.ServerError m => JS.Result a -> m a
checkJS (JS.Error msg) = SE.jsonDecodeError msg
checkJS (JS.Ok x)      = return x
