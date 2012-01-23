{-# LANGUAGE OverloadedStrings #-}

module JsWidget where

import Happstack.Lite
import Happstack.Server.Routing              ( trailingSlash )

import System.Directory
import System.FilePath
import Control.Monad                         ( guard )
import Control.Monad.Trans                   ( liftIO )
import Control.Applicative                   ( optional )
import Data.List                             ( intercalate )
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text.Lazy as T

-- Is this a javascript file or directory within a widgets directory?
widget :: FilePath -> [String] -> ServerPart Response
widget root baseUrl = msum [
      path (\p -> jsModFile root baseUrl (p ++ ".js"))
    , jsMod root baseUrl
    , path (\p -> widget root (baseUrl ++ [p]))
    ]

-- Is there a javascript file named 'index.js' within this directory?
jsMod :: FilePath -> [String] -> ServerPart Response
jsMod root baseUrl = do
      nullDir
      trailingSlash
      jsModFile root baseUrl "index.js"

-- Is there a javascript file here? 
jsModFile :: FilePath -> [String] -> FilePath -> ServerPart Response
jsModFile root baseUrl filename = do
      b <- liftIO (doesFileExist (joinPath (root : url)))
      guard b
      maybeNm <- optional (lookText "main")  -- TODO: Verify this string is a JavaScript identifier
      ok (toResponse (htmlForJsMod baseUrl filename (fmap T.unpack maybeNm)))
   where
      url = baseUrl ++ [filename]

htmlForJsMod :: [String] -> String -> Maybe String -> H.Html
htmlForJsMod baseUrl filename maybeNm = appTemplate $ do
--      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.media "all" ! A.href nineAttr
      H.script ! A.src yoinkAttr  ! A.type_ "text/javascript" $ ""
      H.script ! A.type_ "text/javascript" $ H.toHtml (T.pack yoink)
  where
--      nineAttr = H.toValue (mkPath (mkRelUrl baseUrl ["css", "960.css"]))
      yoinkAttr = H.toValue (mkPath (mkRelUrl baseUrl ["yoink", "yoink.js"]))

      yoink = "\nYOINK.require(['" ++ filename ++ "'], function(M) {\n    "
           ++ setTitle
           ++ reassign
           ++ "function nodeReady(nd){document.body.appendChild(nd);}\n    "
           ++ "var node = typeof M === 'function' ? M({}, nodeReady) : M;\n    "
           ++ "if (node !== undefined) { nodeReady(node); }\n"
           ++ "});\n"

      setTitle = "if (M.title) { document.title = M.title; }\n    "

      reassign = "M = " ++ (maybe "M.main || M" (\nm -> "M."++nm) maybeNm) ++ ";\n    "
 

-- mkRelUrl ["a","b"] ["c","d"] == ["..","..","c","d"]
-- mkRelUrl ["a","b"] ["a","c"] == ["..","c"]
mkRelUrl :: [String] -> [String] -> [String]
mkRelUrl (x:xs) (y:ys) | x == y   = mkRelUrl xs ys 
mkRelUrl baseUrl url              = (replicate (length baseUrl) "..") ++ url

-- Path elements to URL with all forward slashes
mkPath :: [String] -> String
mkPath = intercalate "/"

appTemplate :: H.Html -> H.Html
appTemplate body =
     H.html $ do
       H.head $ do
         H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
       H.body ! A.style "margin: 0; padding: 0" $ body

