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

site :: FilePath -> FilePath -> [FilePath] -> DB.Database -> ServerPart Response
site yoinkDir tagDir modDirs st = msum (moduleDirs ++ staticDirs)
  where
    staticDirs = [ 
          fileServer yoinkDir
        , dir "Tag" (msum [
             JAS.jsAppDirectory tagDir ["Tag"]
           , fileServer tagDir
           ])
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
    maps <- liftIO (mapM (\x -> webAppMap x "") modDirs)
    return $ toResponse (preloadedMods (concat maps))

webAppMap :: FilePath -> String -> IO [(FilePath, Text.Text)]
webAppMap root relDir = do
    nms     <- liftM filterHiddenFiles (getDirectoryContents baseDir)
    dirs    <- filterM (doesDirectoryExist . (baseDir </>)) nms
    fileMap <- mapM (webAppField baseDir relDir) (filter ((== ".js") . takeExtension) nms)
    subMaps <- mapM (webAppMap root . appendPathSegment relDir) dirs
    return (concat (fileMap : subMaps))
  where
    baseDir = root </> relDir

webAppField :: FilePath -> String -> FilePath -> IO (String, Text.Text)
webAppField baseDir relDir nm = do
    cnts <- TextIO.readFile (baseDir </> nm)
    return (appendPathSegment relDir nm, cnts)

filterHiddenFiles :: [FilePath] -> [FilePath]
filterHiddenFiles = filter ((/= ".") . take 1)

appendPathSegment :: String -> String -> String
appendPathSegment   "" nm = nm
appendPathSegment root nm = root ++ "/" ++ nm

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


