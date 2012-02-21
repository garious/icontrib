module Shakefile where

import Development.Shake(need, want, readFileLines, writeFileLines, readFile', doesFileExist, Action, Rules, (*>), system', shake, shakeOptions)
import Development.Shake.FilePath(replaceExtension, dropDirectory1, (<.>))
import Control.Monad(filterM)
import Data.Char(isAlphaNum, isUpper)
import Data.List(nub, sort, isPrefixOf)
import System.Environment(getArgs)

-- Start Shake with our build rules
main :: IO ()
main = do
    args <- getArgs
    shake shakeOptions (rules args)

-- Our build rules go here
rules :: [String] -> Rules ()
rules args = do

    -- What the user wants
    want (obj "Main" : args)

    -- Run the server.  Use: "runghc Shakefile.hs serve"
    "serve" *> \_ -> run (obj "Main")

    -- Build the server.  Use: "runghc Shakefile.hs"
    obj "Main" *> exe

    -- Rules for building haskell executables
    ghcRules


-- Build and run the given executable.  Note: Does not produce a build artifact.
run :: FilePath -> Action ()
run out = do
    need [out]
    system' out []

-- The rules to build Haskell executables
ghcRules :: Rules ()
ghcRules = do
        obj "*.deps" *> depsFile
        obj "*.dep"  *> depFile
        obj "*.hi"   *> interface
        obj "*.o"    *> object

-- Build the given Haskell executable
-- Note: Currently builds all the objects but then uses GHC's "--make" to hunt down the packages.
-- TODO: Grab the packages from cabal
exe :: FilePath -> Action ()
exe out = do
    src <- readFileLines $ replaceExtension out "deps"
    let os = map (obj . moduleToFile "o") $ unobj out : src
    need os
    ghc $ ["--make", "-o",out, "-outputdir", obj ""] ++ [unobj out ++ ".hs"] --os

-- Build a dependency file
depsFile :: FilePath -> Action ()
depsFile out = do
    dep <- readFileLines $ replaceExtension out "dep"
    let xs = map (obj . moduleToFile "deps") dep
    need xs
    ds <- fmap (nub . sort . (++) dep . concat) $ mapM readFileLines xs
    writeFileLines out ds

-- Build a dependency file
depFile :: FilePath -> Action ()
depFile out = do
    src <- readFile' $ unobj $ replaceExtension out "hs"
    let xs = hsImports src
    xs' <- filterM (doesFileExist . moduleToFile "hs") xs
    writeFileLines out xs'

-- Build a Haskell interface file
interface :: FilePath -> Action ()
interface out = do
    need [replaceExtension out "o"]

-- Build an object file from a Haskell source file
object :: FilePath -> Action ()
object out = do
    dep <- readFileLines $ replaceExtension out "dep"
    let hs = unobj $ replaceExtension out "hs"
    need $ hs : map (obj . moduleToFile "hi") dep
    ghc ["-c",hs, "-outputdir", obj "", "-i"++obj ""]


-- Rules that depend on unreleased code in Shake

    --obj ".pkgs" *> \out -> do
    --    src <- readFile' "shake.cabal"
    --    writeFileLines out $ sort $ cabalBuildDepends src

    --addOracle ["ghc-pkg"] $ do
    --    (out,_) <- systemOutput "ghc-pkg" ["list","--simple-output"]
    --    return $ words out

    --addOracle ["ghc-version"] $ do
    --    (out,_) <- systemOutput "ghc" ["--version"]
    --    return [out]

    --addOracle ["ghc-flags"] $ do
    --    pkgs <- readFileLines $ obj ".pkgs"
    --    return $ map ("-package=" ++) pkgs

-- Invoke GHC
ghc :: [String] -> Action ()
ghc args = do
    --askOracle ["ghc-version"]
    --askOracle ["ghc-pkg"]
    --flags <- askOracle ["ghc-flags"]
    let flags = ["-Wall", "-Werror"]
    system' "ghc" $ args ++ flags

moduleToFile :: String -> String -> FilePath
moduleToFile ext xs = map (\x -> if x == '.' then '/' else x) xs <.> ext

-- Map a file to the output directory
obj :: FilePath -> FilePath
obj = ("out/"++)

-- Unmap a file from the output directory
unobj :: FilePath -> FilePath
unobj = dropDirectory1



---------------------------------------------------------------------
-- GRAB INFORMATION FROM FILES

hsImports :: String -> [String]
hsImports xs = [ takeWhile (\y -> isAlphaNum y || y `elem` "._") $ dropWhile (not . isUpper) x
               | x <- lines xs, "import " `isPrefixOf` x]


-- FIXME: Should actually parse the list from the contents of the .cabal file
cabalBuildDepends :: String -> [String]
cabalBuildDepends _ = words "transformers binary unordered-containers parallel-io filepath directory process access-time deepseq"
