
module Programs.PrintFSEvents where

import Control.Effect

import Effects.Filesystem
import Effects.FileWatcher
import Effects.Notice

import Effects.Filesystem.Posix
import Effects.FileWatcher.INotify
import Effects.Notice.Console

import Control.Concurrent

import qualified Data.ByteString as BS

type P sig m = (Monad m, Member (FileWatcher () BS.ByteString) sig, Carrier sig m)

program :: P sig m => m ()
program = watchDir () ("/home/gareth/tmp":: BS.ByteString) $ \event -> 
    putStrLn "Event happened"

runINotify' :: forall sig m a. INotifyImp BS.ByteString sig m
            => Eff (WatcherINotifyC () BS.ByteString m) a -> m a
runINotify' = runINotify

main :: IO ()
main = do
    runM . runConsoleNotice . runFsPosix . runINotify' $ program
    threadDelay 1000000000

