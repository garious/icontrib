{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module Site.Utils where

import Data.Data                             ( Data )
import JSONUtil                              ( jsonDecode )
import Happstack.Server                      ( lookPairs, path )
import Happstack.Server.Monads               ( ServerPartT )
import System.FilePath                       ( takeBaseName )
import Monad                                 ( mzero )
import qualified Data.ByteString.Lazy.Char8  as BS
import qualified Log                         as Log
import SiteError

getBody :: Data a => SiteErrorT a
getBody = do   
    Log.debugM "getBody"
    bd <- lift $ lookPairs
    let 
            --GIANT FREAKING HACK :)
            --wtf cant i get the request body
            from (name, (Right ss)) = name ++ ss
            from (_, (Left _)) = []
            bd' :: String
            bd' = concatMap from bd
    Log.debugShow ("body"::String, bd')
    jsonDecode $ bd'

basename :: ServerPartT IO BS.ByteString
basename = path $ \ (pp::String) -> isext ".json" pp
    where
        isext ee pp
            | (reverse ee) == (take (length ee) $ reverse pp) = return  (BS.pack $ takeBaseName pp)
            | otherwise = mzero



