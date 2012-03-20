{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module Site where
import Control.Monad.IO.Class                ( MonadIO, liftIO )
import Data.Data                             ( Data )
import Control.Monad                         ( liftM )
import Happstack.Server.Monads               ( ServerPartT )
import System.FilePath                       ( takeBaseName )
import Control.Applicative                   ( (<|>) )
import JSONUtil                              ( jsonEncode, jsonDecode )
import qualified Data.ByteString.Lazy        as B
import qualified Data.ByteString.Lazy.Char8  as BS
import qualified Codec.Binary.Url            as Url
import qualified ServerError                 as SE
import ServerError                           ( justOr )
import qualified Login                       as L
import qualified Data.Login                  as L
import qualified Data.JSON                   as J
import qualified CharityInfo                 as C
import qualified Data.CharityInfo            as C
import qualified UserInfo                    as U
import qualified JsWidget                    as JSW
import qualified Network.HTTP                as HTTP
import qualified DB                          as DB
import Monad                                 ( msum, mzero )
import Happstack.Server.SimpleHTTPS          ( simpleHTTPS, TLSConf, tlsPort )
import Happstack.Server                      ( ServerPart
                                             , Response
                                             , path
                                             , lookPairs
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
                                             , lookCookieValue
                                             , mkCookie
                                             , addCookies
                                             , CookieLife(Session)
                                             , simpleHTTP
                                             , nullConf
                                             , port
                                             , decodeBody
                                             , defaultBodyPolicy
                                             , seeOther
                                             )
import qualified Log as Log

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
    liftIO $ Log.debugM "redirecting to ssl" 
    tohttps hn (tlsPort tlsconf)

tohttps :: String -> Int -> ServerPart Response
tohttps hn pn = (seeOther ("https://" ++ hn ++ ":" ++ show pn) (toResponse ()))

site :: DB.Database -> ServerPart Response
site st = msum [ 
      JSW.widget root []
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
      dir "login"  (postF    (loginUser  "auth" st))
    , dir "add"    (postF    (addUser    "auth" st))
    , dir "logout" (postF    (check >>= logOut st))
    , dir "check.json" (get check)
    ]
    where
        check = (checkUser "auth" st)

donorServices:: DB.Database -> ServerPart Response
donorServices st = msum [ 
      dir "update"               (postF  (check >>= (withBody (U.updateInfo st))))
    , dir "get"                  (getF   (check >>= (U.queryByOwner st)))
    , dir "ls"                   (getF   (liftIO $ U.list st))
    , dir "mostInfluential.json" (get    (U.mostInfluential st))
    , (get (basename >>= (U.queryByOwner st . L.Identity)))
    ]
    where
        check = (checkUser "auth" st)

charityServices :: DB.Database -> ServerPart Response
charityServices st = msum [ 
      dir "update"       (postF (check >>= (withBody (C.updateInfo st))))
    , dir "get.json"     (getF  (check >>= (C.queryByOwner st)))
    , dir "popular.json" (get  popular)
    , (get (basename >>= (C.queryByCID st . C.CharityID . BS.unpack)))
    ]
    where
        check :: ServerPartT IO L.Identity
        check = (checkUser "auth" st)
        popular = (U.popularCharities st) >>= (C.toPopular st)

basename :: ServerPartT IO BS.ByteString
basename = path $ \ (pp::String) -> isext ".json" pp
    where
        isext ee pp
            | (reverse ee) == (take (length ee) $ reverse pp) = return  (BS.pack $ takeBaseName pp)
            | otherwise = mzero


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
   page' <- page
   rsp page'

getF :: (Show a, Data a) => ServerPartT IO a -> ServerPartT IO Response
getF page = do 
   method GET
   rq <- askRq
   liftIO $ Log.debugShow ("get"::String, (rqUri rq))
   rv <- SE.catchFail page
   rsp $ rv 

postF :: (Show a, Data a) => ServerPartT IO a -> ServerPartT IO Response
postF page = do 
   method POST
   rq <- askRq
   liftIO $ Log.debugShow ("post"::String, (rqUri rq))
   rv <- SE.catchFail page
   rsp $ rv

homePage :: ServerPart Response
homePage = serveDirectory DisableBrowsing ["index.html"] "public"

fileServer :: FilePath -> ServerPart Response
fileServer dd = do 
    addHeaderM "Pragma" "no-cache"
    serveDirectory DisableBrowsing [] dd

logOut :: MonadIO m => DB.Database -> L.Identity -> m ()
logOut db uid = do 
    liftIO $ Log.debugM $ "logout: " ++ (show uid)
    liftIO $ L.clearIdentityTokens db uid
    liftIO $ Log.debugM "cleared cookies" 

checkUser :: String -> DB.Database -> ServerPartT IO L.Identity
checkUser name db = do 
   liftIO $ Log.debugM "check" 
   cookie <- getCookieValue name
   liftIO $ Log.debugShow cookie
   token <- cookie `justOr` SE.cookieDecodeError
   uid <- L.tokenToIdentity db (L.Token token)
   return uid

getCookieValue :: String -> ServerPartT IO (Maybe B.ByteString)
getCookieValue name = do { val <- lookCookieValue name
                         ; return $ liftM B.pack $ Url.decode val
                         }
                     <|> return Nothing

loginUser :: String -> DB.Database -> ServerPartT IO String
loginUser name db = do 
   liftIO $ Log.debugM "login" 
   (J.UserLogin uid pwd) <- getBody
   token <- L.loginToToken db (L.toIdentity uid) (L.toPassword pwd)
   liftIO $ Log.debugM (show uid)
   let cookie = mkCookie name (Url.encode (L.tokenUnpack token))
   addCookies [(Session, cookie)]
   return $ uid

addUser :: String -> DB.Database -> ServerPartT IO String
addUser name db = do 
   liftIO $ Log.debugM "add" 
   (J.UserLogin uid pwd) <- getBody
   L.addIdentity db (L.toIdentity uid) (L.toPassword pwd)
   loginUser name db

getBody :: (Data b) => ServerPartT IO b
getBody = do   
    liftIO $ Log.debugM "getBody"
    bd <- lookPairs
    let 
            --GIANT FREAKING HACK :)
            --wtf cant i get the request body
            from (name, (Right ss)) = name ++ ss
            from (_, (Left _)) = []
            bd' :: String
            bd' = concatMap from bd
    liftIO $ Log.debugShow ("body"::String, bd')
    jsonDecode $ bd'

rsp :: (Show a, Data a) => a -> ServerPart Response
rsp msg = do
    let json = jsonEncode msg
    liftIO (Log.debugShow json)
    ok $ toResponse $ json

withBody :: Data t => (t1 -> t -> ServerPartT IO b) -> t1 -> ServerPartT IO b
withBody ff uid = do
    bd <- getBody
    ff uid bd


