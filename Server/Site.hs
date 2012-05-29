{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module Site where

import Control.Monad.IO.Class                ( MonadIO, liftIO )
import Control.Monad                         ( liftM )
import qualified Data.Login                  as L
import qualified DB.UserInfo                 as U
import qualified Data.UserInfo               as U
import qualified JsAppServer                 as JAS
import qualified Network.HTTP                as HTTP
import qualified DB.DB                       as DB
import qualified Data.Text                   as Text
import qualified Data.Text.IO                as TextIO
import Control.Monad                         ( msum, filterM )
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
import SiteError                             ( runErrorT, failErrorT, SiteErrorT )
import System.FilePath                       ( (</>), takeExtension )
import System.Directory                      ( getDirectoryContents, doesDirectoryExist )
import Site.Utils                            ( basename, post, get, getBody' )
import qualified Site.Charity               as C
import qualified Site.Stats                 as S

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

site :: FilePath -> [FilePath] -> DB.Database -> ServerPart Response
site yoinkDir modDirs st = msum (moduleDirs ++ staticDirs)
  where
    staticDirs = [ 
          fileServer yoinkDir
        , dir "WebApp.js" (webApp modDirs)
        , dir "auth"    (authServices st)
        , dir "donor"   (donorServices st)
        , dir "charity" (C.charityServices st)
        , dir "stats"   (S.stats st)
        , dir "mirror" $ dir "google" $ dir "jsapi" (redirect (HTTP.getRequest "http://www.google.com/jsapi"))
        ]
    moduleDirs = map (\x -> JAS.jsAppDirectory x []) modDirs ++ map fileServer modDirs

authServices:: DB.Database -> ServerPart Response
authServices st = msum [ 
      dir "login"      (post (runErrorT $ SL.loginUser  st))
    , dir "add"        (post (runErrorT $ SL.addUser    st))
    , dir "logout"     (post (runErrorT $ check >>= (SL.logOut st)))
    , dir "check.json" (get  (runErrorT $ check))
    ]
    where
        check = (SL.checkUser st)

webApp :: [FilePath] -> ServerPart Response
webApp modDirs = do
    cnts <- liftIO (mapM (\root -> webAppMap root "") modDirs)
    return $ toResponse (preloadedMods (concat cnts))

webAppMap :: FilePath -> String -> IO [(FilePath, Text.Text)]
webAppMap root relDir = do
    let baseDir = root </> relDir
    nms <- getDirectoryContents baseDir
    let files = [x | x <- nms, take 1 x /= "." && takeExtension x == ".js"]
    strs <- mapM TextIO.readFile (map (baseDir </>) files)
    let fileMap = zipWith (\nm s -> (if null relDir then nm else relDir ++ "/" ++ nm, s)) files strs

    dirs <- filterM (\nm -> doesDirectoryExist (baseDir </> nm)) (filter (\x -> take 1 x /= ".") nms)
    subMaps <- mapM (\nm -> webAppMap root (if null relDir then nm else relDir ++ "/" ++ nm)) dirs 
    return (concat (fileMap : subMaps))

preloadedMods :: [(FilePath, Text.Text)] -> Text.Text
preloadedMods xs = Text.concat [
    "window.PRELOADED_MODULES = {\n",
    Text.intercalate ",\n" (map wrapFile xs),
    "};\n"
    ]

wrapFile :: (FilePath, Text.Text) -> Text.Text
wrapFile (p, cnts) = Text.concat [
    "\"/",
    Text.pack p,
    "\": (function (Yoink) {\"use strict\";\n",
    cnts,
    "\n})"
    ]

donorServices:: DB.Database -> ServerPart Response
donorServices st = msum [ 
      dir "update"                   (post (runErrorT  $ updateInfo st))
    , dir "mostInfluential.json"     (get  (failErrorT $ U.mostInfluential st))
    , dir "mostInfluentialUser.json" (get  (failErrorT $ mostInfluentialUser st))
    , dir "checkUser.json"           (get  (runErrorT $ checkUserJson st))
    , (get (basename >>= (failErrorT . U.queryByOwner st . L.Identity)))
    ]

mostInfluentialUser :: DB.Database -> SiteErrorT (U.UserInfo)
mostInfluentialUser st = do
    uid <- U.mostInfluential st
    U.queryByOwner st uid


checkUserJson :: DB.Database -> SiteErrorT (U.UserInfo)
checkUserJson st = do
    uid <- SL.checkUser st
    U.queryByOwner st uid

updateInfo :: DB.Database -> SiteErrorT ()
updateInfo st = do
    uid <- SL.checkUser st
    bd <- getBody'
    U.updateInfo st uid bd

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


