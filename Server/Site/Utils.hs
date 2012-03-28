{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module Site.Utils where

import Data.Data                             ( Data )
import JSONUtil                              ( jsonDecode, jsonEncode )
import Happstack.Server                      ( lookPairs, path )
import Happstack.Server.Monads               ( ServerPartT )
import System.FilePath                       ( takeBaseName )
import Monad                                 ( mzero )
import qualified Data.ByteString.Lazy.Char8  as BS
import qualified Log                         as Log
import Happstack.Server                      ( Response
                                             , ok
                                             , toResponse
                                             , askRq
                                             , rqUri
                                             , method
                                             , Method(GET, POST)
                                             )


import SiteError

get :: (Show a, Data a) => ServerPartT IO a -> ServerPartT IO Response
get page = do 
    method GET
    rq <- askRq
    liftIO $ Log.debugShow ("get"::String, (rqUri rq))
    rv <- page
    liftIO $ Log.debugShow ("get response"::String, (show rv))
    rsp rv

post :: (Show a, Data a) => ServerPartT IO a -> ServerPartT IO Response
post page = do 
    method POST
    rq <- askRq
    liftIO $ Log.debugShow ("post"::String, (rqUri rq))
    rv <- page
    liftIO $ Log.debugShow ("post response"::String, (show rv))
    rsp rv

rsp :: (Show a, Data a, MonadIO m) => a -> ServerPartT m Response
rsp msg = do
    let json = jsonEncode msg
    (Log.debugShow json)
    ok $ toResponse $ json

getBody :: Data a => SiteErrorT a
getBody = do   
    bd <- getBody'
    jsonDecode bd

getBody' :: SiteErrorT String
getBody' = do   
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
    return bd'

basename :: ServerPartT IO BS.ByteString
basename = path $ \ (pp::String) -> isext ".json" pp
    where
        isext ee pp
            | (reverse ee) == (take (length ee) $ reverse pp) = return  (BS.pack $ takeBaseName pp)
            | otherwise = mzero



withBody :: Data t => (t1 -> t -> ErrorT String (ServerPartT IO) b) -> t1 -> ErrorT String (ServerPartT IO) b
withBody ff uid = do bd <- getBody; ff uid bd
