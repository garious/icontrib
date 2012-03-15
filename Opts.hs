module Opts where
    
import System.Console.GetOpt

data Flag 
 = DbDir String
 | HttpPort Int
   deriving Show

data Options = Options {
   dbDir :: String,
   httpPort :: Int
}
   deriving (Show, Eq)

options :: [OptDescr Flag]
options = [
   Option []        ["dbdir"]   (ReqArg DbDir "DIR")  "directory for database files"
 , Option []        ["port"]    (ReqArg (HttpPort . read) "NUM") "HTTP port"
 ]

getOptions :: [String] -> IO Options
getOptions argv = do
    case getOpt Permute options argv of
       (o,_,[]  ) -> return $ Options {
                       dbDir = headDef "private/db" [x | DbDir x <- o]
                     , httpPort = headDef 8000 [x | HttpPort x <- o]
                     }
       (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

  where
     header = "Usage: icontrib [OPTION...] files..."

     -- returns either 'head' or a default
     headDef _ (x:_) = x
     headDef x []    = x
