{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module Site where
import Control.Monad.IO.Class                ( MonadIO, liftIO )
import Control.Monad                         ( liftM )
import qualified Data.Login                  as L
import qualified DB.UserInfo                 as U
import qualified JsWidget                    as JSW
import qualified Network.HTTP                as HTTP
import qualified DB.DB                       as DB
import Monad                                 ( msum )
import Happstack.Server.SimpleHTTPS          ( simpleHTTPS, TLSConf, tlsPort )
import Happstack.Server                      ( ServerPart
                                             , Response
                                             , toResponse
                                             , dir
                                             , setResponseCode 
                                             , serveDirectory
                                             , Browsing(DisableBrowsing)
                                             , setHeaderM 
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
import Site.Utils                            ( basename, withBody, post, get )
import qualified Site.Charity               as C

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
    , fileServer "Client/Darwin_Debug/ship"
    , fileServer "yoink"
    , fileServer root
    , fileServer "private/images"
    , dir "auth"    (authServices st)
    , dir "donor"   (donorServices st)
    , dir "charity" (C.charityServices st)
    , dir "mirror" $ dir "google" $ dir "jsapi" (redirect (HTTP.getRequest "http://www.google.com/jsapi"))
    ]
  where
    root = "Client"

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
        check = (SL.checkUser st)

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

fileServer :: FilePath -> ServerPart Response
fileServer dd = do 
    addHeaderM "Pragma" "no-cache"
    serveDirectory DisableBrowsing [] dd


