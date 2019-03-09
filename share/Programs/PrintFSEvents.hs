
module Programs.PrintFSEvents where

import Control.Effect

import Effects.Filesystem
import Effects.FileWatcher
import Effects.Notice

import Effects.Filesystem.Posix
import Effects.FileWatcher.INotify
import Effects.Notice.Console

import Control.Monad.IO.Class
import Control.Concurrent

import qualified Data.ByteString as BS

type P sig m = (MonadIO m, Member (FileWatcher () BS.ByteString) sig, Carrier sig m)
  
program :: P sig m => m ()
program = do
  watchDir () ("/home/gareth/tmp":: BS.ByteString) $ \event -> print event
  liftIO $ threadDelay 1000000000

runINotify' :: forall sig m a. INotifyImp BS.ByteString sig m
            => Eff (WatcherINotifyC () BS.ByteString m) a -> m a
runINotify' = runINotify

main :: IO ()
main = runM . runConsoleNotice . runFsPosix . runINotify' $ program

