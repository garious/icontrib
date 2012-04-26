{-# LANGUAGE OverloadedStrings #-}

-- JsCache is used to concatenate Yoink modules

import qualified System.Environment as Env
import qualified Data.Text          as Text
import qualified Data.Text.IO       as TextIO


main :: IO ()
main = do
    args <- Env.getArgs
    let (outnm, files) = parseArgs args

    strs <- mapM TextIO.readFile files
    let cnts = preloadedMods (zip files strs)

    case outnm of
        Nothing -> putStr (Text.unpack cnts)
        Just nm -> TextIO.writeFile nm cnts

parseArgs :: [String] -> (Maybe FilePath, [FilePath])
parseArgs ("-o":p:xs) = (Just p, xs)
parseArgs xs          = (Nothing, xs)


preloadedMods :: [(FilePath, Text.Text)] -> Text.Text
preloadedMods xs = Text.concat [
    "var PRELOADED_MODULES = {\n",
    Text.intercalate ",\n" (map wrapFile xs),
    "};\n"
    ]

wrapFile :: (FilePath, Text.Text) -> Text.Text
wrapFile (p, cnts) = Text.concat [
    "\"/",
    Text.pack p,
    "\": (function (baseUrl, define, require, params) {\"use strict\";\n",
    cnts,
    "\n})"
    ]

