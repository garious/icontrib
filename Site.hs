{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module Site where

import Control.Monad.Trans                   ( liftIO, lift )
import Data.Acid                             ( AcidState )
import Data.Data                             ( Data )
import Control.Monad                         ( liftM )
import Char                                  ( chr )
import Control.Monad.Error                   ( runErrorT, ErrorT )
import Happstack.Server.Monads               ( ServerPartT )
import System.FilePath                       ( takeBaseName )
import Control.Applicative                   ( (<|>) )
import JSONUtil                              ( jsonEncode, jsonDecode )
import qualified Data.ByteString.Lazy        as B
import qualified Data.ByteString.Lazy.Char8  as BS
import qualified Codec.Binary.Url            as Url
import qualified ServerError                 as SE
import qualified Account                     as A
import qualified CharityInfo                 as C
import qualified UserInfo                    as U
import qualified JsWidget                    as JSW
import qualified Network.HTTP                as HTTP
import Monad                                 ( msum, mzero )
import Happstack.Server                      ( ServerPart
                                             , Response
                                             , path
                                             , lookPairs
                                             , ok
                                             , internalServerError
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
                                             , tls
                                             , TLSConf
                                             , decodeBody
                                             , defaultBodyPolicy
                                             , http
                                             , https
                                             , seeOther
                                             , tlsPort
                                             )
import qualified Log as Log
data Site = Site { userAccounts ::  AcidState A.Database
                 , charityInfo :: AcidState C.Database
                 , userInfo :: AcidState U.Database
                 }

serve :: Maybe (String, TLSConf) -> Int -> ServerPart Response -> IO ()
serve mtls portnum part = 
    let 
        ramQuota  =  1000000
        diskQuota = 20000000
        tmpDir    = "/tmp/"
    in  simpleHTTP (nullConf { port = portnum, tls = (liftM snd) mtls }) $ do 
            decodeBody (defaultBodyPolicy tmpDir diskQuota ramQuota (ramQuota `div` 10))
            servepart mtls part
    
tohttps :: String -> Int -> ServerPart Response
tohttps hn pn = (seeOther ("https://" ++ hn ++ ":" ++ show pn) (toResponse ()))

servepart :: Maybe (String, TLSConf) -> ServerPart Response -> ServerPart Response
servepart (Just (hn, tlsconf)) part = msum [ do http 
                                                (tohttps hn (tlsPort tlsconf))
                                           , do https 
                                                part
                                           ]
servepart Nothing part = part

site :: Site -> ServerPart Response
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

authServices:: Site -> ServerPart Response
authServices st = msum [ 
      dir "login"  (post    (loginUser  "auth" (userAccounts st)))
    , dir "add"    (post    (addUser    "auth" (userAccounts st)))
    , dir "logout" (get     (check >>= logOut (userAccounts st)))
    , dir "check"  (get     check)
    ]
    where
        check = (checkUser "auth" (userAccounts st))

donorServices:: Site -> ServerPart Response
donorServices st = msum [ 
      dir "update"               (post  (check >>= (withBody (U.updateInfo (userInfo st)))))
    , dir "get"                  (get   (check >>= (U.lookupInfo (userInfo st))))
    , dir "ls"                   (get   (liftIO $ U.list (userInfo st)))
    , dir "mostInfluential.json" (getf  (U.mostInfluential (userInfo st)))
    , (getf (lift basename >>=  (U.lookupInfo (userInfo st))))
    ]
    where
        check = (checkUser "auth" (userAccounts st))
        isext ee pp
            | (reverse ee) == (take (length ee) $ reverse pp) = return  (BS.pack $ takeBaseName pp)
            | otherwise = mzero
        basename = path $ \ (pp::String) -> isext ".json" pp

charityServices :: Site -> ServerPart Response
charityServices st = msum [ 
      dir "update" (post (check >>= (withBody (C.updateInfo (charityInfo st)))))
    , dir "get"    (get  (check >>= (C.lookupInfo (charityInfo st))))
    ]
    where
        check = (checkUser "auth" (userAccounts st))

withBody :: Data t => (t1 -> t -> IO b) -> t1 -> ErrorT SE.ServerError (ServerPartT IO) b
withBody ff uid = do
    bd <- getBody
    liftIO $ ff uid bd

redirect ::  HTTP.Request_String -> ServerPart Response
redirect req = do
   hrsp <- liftIO $ liftM check $ (HTTP.simpleHTTP req)
   setResponseCode $ fromCode $ HTTP.rspCode hrsp
   mapM_ (\ (HTTP.Header name val) -> setHeaderM (show name) val) $ HTTP.rspHeaders hrsp 
   return $ toResponse $ HTTP.rspBody hrsp
   where
      fromCode (x,y,z) = x * 100 + y * 10 + z
      check (Left err)  = error (show err) 
      check (Right rr)  = rr

getf :: (Show a, Data a) => ErrorT SE.ServerError (ServerPartT IO) a -> ServerPartT IO Response
getf page = do 
   method GET
   rq <- askRq
   liftIO $ Log.debugShow ("get"::String, (rqUri rq))
   rv <- runErrorT page
   case(rv) of
        (Left ee) -> internalServerError (toResponse $ show ee)
        (Right res) -> rsp $ res 

get :: (Show a, Data a) => ErrorT SE.ServerError (ServerPartT IO) a -> ServerPartT IO Response
get page = do 
   method GET
   rq <- askRq
   liftIO $ Log.debugShow ("get"::String, (rqUri rq))
   rv <- runErrorT page
   rsp $ rv 

post :: (Show a, Data a) => ErrorT SE.ServerError (ServerPartT IO) a -> ServerPartT IO Response
post page = do 
   method POST
   rq <- askRq
   liftIO $ Log.debugShow ("post"::String, (rqUri rq))
   rv <- (runErrorT page)
   rsp $ rv

homePage :: ServerPart Response
homePage = serveDirectory DisableBrowsing ["index.html"] "public"

fileServer :: FilePath -> ServerPart Response
fileServer dd = do 
    addHeaderM "Pragma" "no-cache"
    serveDirectory DisableBrowsing [] dd

logOut :: AcidState A.Database ->  A.UserID -> ErrorT SE.ServerError (ServerPartT IO) ()
logOut db uid = do 
    liftIO $ Log.debugM $ "logout: " ++ (toS uid)
    liftIO $ A.clearUserCookie db uid
    liftIO $ Log.debugM "cleared cookies" 

checkUser :: String -> AcidState A.Database -> ErrorT SE.ServerError (ServerPartT IO) A.UserID
checkUser name db = do 
   liftIO $ Log.debugM "check" 
   cookie <- lift $ getCookieValue name
   liftIO $ Log.debugShow cookie
   token <- SE.checkMaybe SE.CookieDecode $ cookie 
   uid <- A.cookieToUser db token
   return uid

getCookieValue :: String -> ServerPartT IO (Maybe B.ByteString)
getCookieValue name = do { val <- lookCookieValue name
                         ; return $ liftM B.pack $ Url.decode val
                         }
                     <|> return Nothing

loginUser :: String -> AcidState A.Database -> ErrorT SE.ServerError (ServerPartT IO) A.UserID
loginUser name db = do 
   liftIO $ Log.debugM "login" 
   (A.UserLogin uid pwd) <- getBody
   token <- A.loginToCookie db (uid) (pwd)
   liftIO $ Log.debugM (show uid)
   let cookie = mkCookie name (Url.encode (B.unpack token))
   lift $ addCookies [(Session, cookie)]
   return $ uid

addUser :: String -> AcidState A.Database -> ErrorT SE.ServerError (ServerPartT IO) A.UserID
addUser name db = do 
   liftIO $ Log.debugM "add" 
   (A.UserLogin uid pwd) <- getBody
   A.addUser db (uid) (pwd)
   loginUser name db
   
getBody :: Data a => ErrorT SE.ServerError (ServerPartT IO) a 
getBody = do   
    liftIO $ Log.debugM "getBody"
    bd <- lift $ lookPairs
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

toS :: B.ByteString -> String
toS ss = map (chr . fromIntegral) $ B.unpack ss

