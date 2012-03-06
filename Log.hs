module Log where

import System.Log.Logger(updateGlobalLogger, Priority(DEBUG), setLevel, addHandler)
import System.Log.Handler.Simple
import System.Log.Handler(setFormatter)
import System.Log.Formatter
import qualified System.Log.Logger as Logger( debugM )

app :: String
app = "icontrib.org" 

start :: IO ()
start =  do
    updateGlobalLogger app (setLevel DEBUG)
    h <- fileHandler "debug.log" DEBUG >>= \lh -> return $
        setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger app (addHandler h)

debugM :: String -> IO ()
debugM = Logger.debugM app

debugShow :: Show a => a -> IO ()
debugShow vv = Logger.debugM app (show vv)
