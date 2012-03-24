{-# LANGUAGE FlexibleContexts #-}
module JSONUtil where

import Data.Data                             ( Data )
import Control.Monad.Identity                ( runIdentity )
import Data.List                             ( foldl' )
import qualified Text.JSON.Generic           as JS
import qualified Text.JSON.String            as JSS
import SiteError


runJsonDecode :: Data b => String -> Either String b
runJsonDecode str = runIdentity $ runErrorT $ jsonDecode str

jsonDecodeE :: Data b => String -> b
jsonDecodeE str = JS.decodeJSON str

jsonDecode :: (MonadError String m, Data b) => String -> m b
jsonDecode str = do 
    val <- jsonParse str 
    checkJS $ JS.fromJSON $ val 

runJsonParse :: String -> Either String JS.JSValue
runJsonParse str = runIdentity $ runErrorT $ jsonParse str

jsonParse :: (MonadError String m) => String -> m JS.JSValue
jsonParse str = do
    let check (Left msg) = jsonParseError msg
        check (Right j)  =  return j
    check $ JSS.runGetJSON JS.readJSValue str

jsonParseError :: MonadError String m => String -> m a
jsonParseError str  = fail ("JSONParseError: " ++ str)

jsonDecodeError :: MonadError String m => String -> m a
jsonDecodeError str = fail ("JSONDecodeError: " ++ str)

jsonEncode :: Data a => a -> String 
jsonEncode = JS.encodeJSON

jsonUpdate :: (MonadError String m, Data b, Data a) => a -> String -> m b 
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

checkJS :: MonadError String m => JS.Result a -> m a
checkJS (JS.Error msg) = jsonDecodeError msg
checkJS (JS.Ok x)      = return x
