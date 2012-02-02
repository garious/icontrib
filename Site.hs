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
import qualified Data.ByteString.Lazy        as B
import qualified Data.ByteString.Lazy.Char8  as BS
import qualified Codec.Binary.Url            as Url
import qualified Text.JSON.Generic           as JS
import qualified ServerError                 as SE
import qualified Account                     as A
import qualified CharityInfo                 as C
import qualified UserInfo                    as U
import qualified JsWidget                    as JSW
import qualified Network.HTTP                as HTTP
import Happstack.Server(askRq, rqUri, lookPairs)
import Happstack.Lite

data Site = Site { userAccounts ::  AcidState A.Database
                 , charityInfo :: AcidState C.Database
                 , userInfo :: AcidState U.Database
                 }

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
            | (reverse ee) == (take (length ee) $ reverse pp) = return (BS.pack (takeBaseName pp))
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
   liftIO $ print ("get"::String, (rqUri rq))
   rv <- runErrorT page
   case(rv) of
        (Left ee) -> internalServerError (toResponse $ show ee)
        (Right res) -> rsp $ res 

get :: (Show a, Data a) => ErrorT SE.ServerError (ServerPartT IO) a -> ServerPartT IO Response
get page = do 
   method GET
   rq <- askRq
   liftIO $ print ("get"::String, (rqUri rq))
   rv <- runErrorT page
   rsp $ rv 

post :: (Show a, Data a) => ErrorT SE.ServerError (ServerPartT IO) a -> ServerPartT IO Response
post page = do 
   method POST
   rq <- askRq
   liftIO $ print ("post"::String, (rqUri rq))
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
    liftIO $ putStrLn $ "logout: " ++ (toS uid)
    liftIO $ A.clearUserCookie db uid
    liftIO $ putStrLn "cleared cookies" 

checkUser :: String -> AcidState A.Database -> ErrorT SE.ServerError (ServerPartT IO) A.UserID
checkUser name db = do 
   liftIO $ putStrLn "check" 
   cookie <- lift $ getCookieValue name
   liftIO $ print cookie
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
   liftIO $ putStrLn "login" 
   (A.UserLogin uid pwd) <- getBody
   token <- A.loginToCookie db (uid) (pwd)
   liftIO $ putStrLn (show uid)
   let cookie = mkCookie name (Url.encode (B.unpack token))
   lift $ addCookies [(Session, cookie)]
   return $ uid

addUser :: String -> AcidState A.Database -> ErrorT SE.ServerError (ServerPartT IO) A.UserID
addUser name db = do 
   liftIO $ putStrLn "add" 
   (A.UserLogin uid pwd) <- getBody
   A.addUser db (uid) (pwd)
   loginUser name db
   
getBody :: Data a => ErrorT SE.ServerError (ServerPartT IO) a 
getBody = do   
    liftIO $ putStrLn "getBody"
    bd <- lift $ lookPairs
    let 
            decode a = SE.checkMaybe (SE.JSONDecodeError a) $ jsonDecode a 
            --GIANT FREAKING HACK :)
            --wtf cant i get the request body
            from (name, (Right ss)) = name ++ ss
            from (_, (Left _)) = []
            bd' :: String
            bd' = concatMap from bd
    liftIO $ print ("body"::String, bd')
    decode $ bd'

jsonDecode :: Data a => String -> Maybe a
jsonDecode a = (Just $ JS.decodeJSON a) <|> Nothing

jsonEncode :: Data a => a -> String 
jsonEncode = JS.encodeJSON

rsp :: (Show a, Data a) => a -> ServerPart Response
rsp msg = do
    let json = jsonEncode msg
    liftIO (print json)
    ok $ toResponse $ json

toS :: B.ByteString -> String
toS ss = map (chr . fromIntegral) $ B.unpack ss

