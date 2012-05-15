module Opts where
    
import System.Console.GetOpt

data Flag 
 = DbDir FilePath
 | ModDir FilePath
 | HttpPort Int
 | Ssl
   deriving Show

data Options = Options {
   dbDir      :: FilePath
 , modDirs    :: [FilePath]
 , httpPort   :: Int
 , ssl        :: Bool
}
   deriving (Show, Eq)

options :: [OptDescr Flag]
options = [
   Option []        ["dbdir"]   (ReqArg DbDir "DIR")  "directory for database files"
 , Option []        ["moddir"]  (ReqArg ModDir "DIR")  "directory for frontend files"
 , Option []        ["port"]    (ReqArg (HttpPort . read) "NUM") "HTTP port"
 , Option []        ["ssl"]     (NoArg Ssl) "Redirect HTTP requests to SSL"
 ]

getOptions :: [String] -> IO Options
getOptions argv = do
    case getOpt Permute options argv of
       (os,_,[]  ) -> return $ Options {
                       dbDir = headDef "private/db" [x | DbDir x <- os]
                     , modDirs = idDef ["public"] [x | ModDir x <- os]
                     , httpPort = headDef 8000 [x | HttpPort x <- os]
                     , ssl = not (null [Ssl | Ssl <- os])
                     }
       (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

  where
     header = "Usage: icontrib [OPTION...] files..."

     -- returns either 'head' or a default
     headDef x []    = x
     headDef _ (x:_) = x

     -- returns either the same list or a singleton list with the default
     idDef defs [] = defs
     idDef _ xs    = xs
