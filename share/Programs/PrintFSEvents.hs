
module Programs.PrintFSEvents where

import Control.Effect

import Effects.Filesystem
import Effects.FileWatcher
import Effects.Notice
import Effects.Async

import Effects.Filesystem.Posix
import Effects.FileWatcher.INotify
import Effects.Notice.Console
import Effects.Async.NatTrans

import Control.Monad.IO.Class
import Control.Concurrent

import Data.Proxy
  
import qualified Data.ByteString as BS

type P sig m = ( MonadIO m
               , Member (FileWatcher () BS.ByteString) sig
               , Member (AsyncEff IO Int) sig
               , Carrier sig m)
  
program :: P sig m => m ()
program = do
  watchDir () ("/home/gareth/tmp" :: BS.ByteString) $ \event -> print event
  soon4 <-
    forkEff $ do
       print "hello"
       print "world"
       return (4 :: Int)
  four <- waitEff soon4
  liftIO $ print four
  liftIO $ threadDelay 1000000000

runINotify' :: forall sig m a. INotifyC BS.ByteString sig m
            => Eff (WatcherINotifyC () BS.ByteString m) a
            -> m a
runINotify' = runINotify


main :: IO ()
main = runM
  . runNatTrans (Proxy :: Proxy Int) (id :: forall x. IO x -> IO x)
  . runConsoleNotice
  . runFsPosix
  . runINotify' $ program

