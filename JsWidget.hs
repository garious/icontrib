{-# LANGUAGE OverloadedStrings #-}

module JsWidget where

import Happstack.Lite

import System.Directory
import System.FilePath
import Control.Monad                         ( guard )
import Control.Monad.Trans                   ( liftIO )
import Control.Applicative                   ( optional )
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text.Lazy as T

-- Is this a javascript file or directory within a widgets directory?
widget :: FilePath -> FilePath -> ServerPart Response
widget root baseUrl = msum [
      nullDir >> jsMod root baseUrl
    , path (jsModDir root baseUrl)
    ]

-- Is this a javascript file within a widgets directory?
jsMod :: FilePath -> FilePath -> ServerPart Response
jsMod root baseUrl = do
      b <- liftIO (doesFileExist (root </> fp))
      guard b
      maybeNm <- optional (lookText "main")
      ok (toResponse (htmlForJsMod fp (fmap T.unpack maybeNm)))
  where
      -- Don't use </> here.  Breaks URLs on Windows!
      fp | null baseUrl = "index.js"
         | otherwise    = baseUrl ++ "/index.js"

htmlForJsMod :: FilePath -> Maybe String -> H.Html
htmlForJsMod fp maybeNm = appTemplate $ do
      H.base ! A.href (H.toValue fp) $ ""
      H.link ! A.rel "stylesheet" ! A.type_ "text/less" ! A.media "all" ! A.href "/css/main.less"
      H.script ! A.src "/yoink/yoink.js" ! A.type_ "text/javascript" $ ""
      H.script ! A.type_ "text/javascript" $ H.toHtml (T.pack yoink)
  where
      yoink = "\nYOINK.resourceLoader().getResource('/"
           ++ fp
           ++ "', function(M) {\n    "
           ++ setTitle
           ++ reassign
           ++ "document.body.appendChild(typeof M === 'function' ? M() : M);\n});\n"

      setTitle = "if (M.title) { document.title = M.title; };\n    "

      reassign = "M = " ++ (maybe "M.main || M" (\nm -> "M."++nm) maybeNm) ++ ";\n    "
 

appTemplate :: H.Html -> H.Html
appTemplate body =
     H.html $ do
       H.head $ do
         H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
       H.body body

-- Is this a directory within a widgets directory?
jsModDir :: FilePath -> FilePath -> String -> ServerPart Response
jsModDir root baseUrl nm = do
      b <- liftIO (doesDirectoryExist (root </> fp))
      guard b
      widget root fp
  where
      -- Don't use </> here.  Breaks URLs on Windows!
      fp | null baseUrl = nm
         | otherwise    = baseUrl ++ "/" ++ nm

