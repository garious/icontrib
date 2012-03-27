{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module Site where
import Control.Monad.IO.Class                ( MonadIO, liftIO )
import Data.Data                             ( Data )
import Control.Monad                         ( liftM )
import JSONUtil                              ( jsonEncode )
import qualified Data.ByteString.Lazy.Char8  as BS
import qualified Data.Login                  as L
import qualified DB.CharityInfo              as C
import qualified Data.CharityInfo            as C
import qualified DB.UserInfo                 as U
import qualified JsWidget                    as JSW
import qualified Network.HTTP                as HTTP
import qualified DB.DB                       as DB
import Monad                                 ( msum )
import Happstack.Server.SimpleHTTPS          ( simpleHTTPS, TLSConf, tlsPort )
import Happstack.Server.Monads               ( ServerPartT )
import Happstack.Server                      ( ServerPart
                                             , Response
                                             , ok
                                             , toResponse
                                             , askRq
                                             , rqUri
                                             , dir
                                             , setResponseCode 
                                             , serveDirectory
                                             , Browsing(DisableBrowsing)
                                             , setHeaderM 
                                             , method
                                             , Method(GET, POST)
                                             , addHeaderM
                                             , simpleHTTP
                                             , nullConf
                                             , port
                                             , decodeBody
                                             , defaultBodyPolicy
                                             , seeOther
                                             )
import qualified Log as Log
import qualified Site.Login as SL
import SiteError                             ( runErrorT, failErrorT )
import Site.Utils


serve :: Either TLSConf Int -> ServerPart Response -> IO ()
serve (Right pn) part =
    let 
        ramQuota  =  1000000
        diskQuota = 20000000
        tmpDir    = "/tmp/"
    in  simpleHTTP (nullConf { port = pn}) $ do 
            decodeBody (defaultBodyPolicy tmpDir diskQuota ramQuota (ramQuota `div` 10))
            part

serve (Left tlsconf) part =
    let
        ramQuota  =  1000000
        diskQuota = 20000000
        tmpDir    = "/tmp/"
    in  simpleHTTPS tlsconf $ do
            decodeBody (defaultBodyPolicy tmpDir diskQuota ramQuota (ramQuota `div` 10))
            part

redirectToSSL :: TLSConf -> String -> Int -> IO ()
redirectToSSL tlsconf hn pn = simpleHTTP (nullConf { port = pn }) $ do
    Log.debugM "redirecting to ssl" 
    tohttps hn (tlsPort tlsconf)

tohttps :: String -> Int -> ServerPart Response
tohttps hn pn = (seeOther ("https://" ++ hn ++ ":" ++ show pn) (toResponse ()))

site :: DB.Database -> ServerPart Response
site st = msum [ 
      JSW.widget root []
    , fileServer "public/Darwin_Debug/ship"
    , fileServer "yoink"
    , fileServer root
    , dir "auth"    (authServices st)
    , dir "donor"   (donorServices st)
    , dir "charity" (charityServices st)
    , dir "mirror" $ dir "google" $ dir "jsapi" (redirect (HTTP.getRequest "http://www.google.com/jsapi"))
    ]
  where
    root = "public"

authServices:: DB.Database -> ServerPart Response
authServices st = msum [ 
      dir "login"      (post (runErrorT $ SL.loginUser  st))
    , dir "add"        (post (runErrorT $ SL.addUser    st))
    , dir "logout"     (post (runErrorT $ check >>= (SL.logOut st)))
    , dir "check.json" (get  (runErrorT $ check))
    ]
    where
        check = (SL.checkUser st)


donorServices:: DB.Database -> ServerPart Response
donorServices st = msum [ 
      dir "update"               (post (runErrorT  $ check >>= (withBody (U.updateInfo st))))
    , dir "mostInfluential.json" (get  (failErrorT $ U.mostInfluential st))
    , (get (basename >>= (failErrorT . U.queryByOwner st . L.Identity)))
    ]
    where
        withBody ff uid = do 
            bd <- getBody
            ff uid bd
        check = (SL.checkUser st)

charityServices :: DB.Database -> ServerPart Response
charityServices st = msum [ 
      dir "update"       (post (runErrorT $ check >>= (withBody (C.updateInfo st))))
    , dir "get.json"     (get  (runErrorT $ check >>= (C.queryByOwner st)))
    , dir "popular.json" (get  popular)
    , (get (basename >>= (failErrorT . C.queryByCID st . C.CharityID . BS.unpack)))
    ]
    where
        withBody ff uid = do bd <- getBody; ff uid bd
        check = (SL.checkUser st)
        popular = (U.popularCharities st) >>= (C.toPopular st)

redirect ::  HTTP.Request_String -> ServerPart Response
redirect req = do
   hrsp <- liftIO $ liftM check $ (HTTP.simpleHTTP req)
   setResponseCode $ fromCode $ HTTP.rspCode hrsp
   mapM_ (\ (HTTP.Header name val) -> setHeaderM (show name) val) $ HTTP.rspHeaders hrsp 
   return $ toResponse $ HTTP.rspBody hrsp
   where
      fromCode (x,y,z) = x * 100 + y * 10 + z
      check (Left ee)  = error (show ee) 
      check (Right rr)  = rr

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

fileServer :: FilePath -> ServerPart Response
fileServer dd = do 
    addHeaderM "Pragma" "no-cache"
    serveDirectory DisableBrowsing [] dd

rsp :: (Show a, Data a, MonadIO m) => a -> ServerPartT m Response
rsp msg = do
    let json = jsonEncode msg
    (Log.debugShow json)
    ok $ toResponse $ json

