module Log where

import System.Log.Logger(updateGlobalLogger, Priority(DEBUG), setLevel, addHandler)
import System.Log.Handler.Simple
import System.Log.Handler(setFormatter)
import System.Log.Formatter
import qualified System.Log.Logger as Logger( debugM )
import Control.Monad.IO.Class                ( MonadIO, liftIO )

app :: String
app = "icontrib.org" 

start :: IO ()
start =  do
    updateGlobalLogger app (setLevel DEBUG)
    h <- fileHandler "debug.log" DEBUG >>= \lh -> return $
        setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger app (addHandler h)

debugM :: MonadIO m => String -> m ()
debugM str = liftIO $ Logger.debugM app str

debugShow :: MonadIO m => Show a => a -> m ()
debugShow vv = liftIO $ Logger.debugM app (show vv)
