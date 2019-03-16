
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

import Control.Effect.Sum
import Control.Monad.IO.Class
import Control.Concurrent

import Data.Proxy
  
import qualified Data.ByteString as BS

type AsyncC = Eff (ConsoleNotice (Eff (LiftC IO)))

type P sig m = ( MonadIO m
               , Member (FileWatcher () BS.ByteString) sig
               , Member (AsyncEff AsyncC) sig
               , Member Notice sig
               , Carrier sig m)

subprogram :: Eff (ConsoleNotice (Eff (LiftC IO))) Int
subprogram = do
  sendNotice "hello"
  sendNotice "world"
  return (4 :: Int)

program :: P sig m => m ()
program = do
  watchDir () ("/home/gareth/tmp" :: BS.ByteString) $ \event -> print event
  soon4 <- forkEff subprogram
  four <- waitEff soon4
  sendNotice "Hello this is a notice"
  liftIO $ print four
  liftIO $ threadDelay 1000000000

runINotify' :: forall sig m a. INotifyC BS.ByteString sig m
            => Eff (WatcherINotifyC () BS.ByteString m) a
            -> m a
runINotify' = runINotify

runSubprogram :: AsyncC a -> IO a
runSubprogram = runM . runConsoleNotice . interpret

main :: IO ()
main = runM
  . runNatTrans runSubprogram
  . runConsoleNotice
  . runFsPosix
  . runINotify' $ program

