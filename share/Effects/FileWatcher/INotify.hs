
{-# LANGUAGE UndecidableInstances #-}

module Effects.FileWatcher.INotify where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Exception (try, SomeException)

import           Control.Effect
import           Control.Effect.Reader
import           Control.Effect.Carrier
import           Control.Effect.Sum

import           Effects.Filesystem
import           Effects.FileWatcher

import           System.INotify                 ( INotify, isDirectory, filePath, maybeFilePath )
import qualified System.INotify                as INotify

newtype WatcherINotify hDir fp m a = WatcherINotify { runWatcherINotify :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (MonadIO m, Member (Reader INotify) sig, Member Filesystem sig, Carrier sig m) 
         => Carrier (FileWatcher hDir fp :+: sig) (WatcherINotify hDir fp m) where
  ret = pure
  eff = handleSum (WatcherINotify . eff . handleCoercible) $ cases 
      where cases = \case
              WatchDir hDir fp callback k -> inotifyWatchDir hDir fp callback >> k

type INotifyImp sig m = (MonadIO m, Member (Reader INotify) sig, Member Filesystem sig, Carrier sig m) 

inotifyWatchDir :: INotifyImp sig m => hDir -> fp -> (FileEvent hDir fp -> IO ()) -> m ()
inotifyWatchDir hDir fp callback = do
    inotify <- ask
    undefined (inotify :: INotify) 
    return ()


inotifyWatchDirectory :: INotifyImp sig m => hDir -> fp -> (FileEvent hDir fp -> IO ()) -> m INotify.WatchDescriptor
inotifyWatchDirectory hDir fp callback = do
  let watchTypes = [ INotify.Modify
                   , INotify.Attrib
                   , INotify.Move
                   , INotify.MoveOut
                   , INotify.Delete
                   , INotify.Create]
  inotify <- ask
  watchDescriptor <- liftIO $ INotify.addWatch inotify watchTypes fp $ 
    handleEvent hDir fp callback 

  diskFiles <- listDirectory fp

  forM_ diskFiles $ \filename -> do
    let filepath = thisDir </> filename
    isDirectory <- doesDirectoryExist filepath
    if isDirectory
      then void $ inotifyWatchDirectory (error "no sub hdir yet") filepath callback
      else liftIO $ callback $ CreatedFile hDir filepath
  
  return watchDescriptor


handleEvent :: hDir -> fp -> (FileEvent hDir fp -> IO ()) -> INotify.Event -> IO ()
handleEvent hDir fp callback event =
  try handler >>= \case
    Left  e -> liftIO $ print (e :: SomeException)
    Right _ -> return ()
 where
  handler = do
    case event of
      INotify.Created { isDirectory, filePath = filename } -> do
        if isDirectory
          then callback $ CreatedDir hDir fp
          else callback $ CreatedFile hDir fp

      INotify.Modified { maybeFilePath } -> do
        case maybeFilePath of
          Nothing -> return ()
          Just filename -> callback $ ModifiedFile hDir filename 

      INotify.Deleted { filePath } -> return ()

      _ -> return ()

