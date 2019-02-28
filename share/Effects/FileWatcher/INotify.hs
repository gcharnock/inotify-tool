
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

newtype WatcherINotifyC hDir fp m a = WatcherINotifyC { runWatcherINotifyC :: INotify.INotify -> m a }
  deriving (Functor)

instance (MonadIO m, Member (Filesystem fp) sig, Carrier sig m) 
         => Carrier (FileWatcher hDir fp :+: sig) (WatcherINotifyC hDir fp m) where
  ret a = WatcherINotifyC $ \_ -> return a
  eff op = WatcherINotifyC $ \inotify ->
               let cases = \case
                      WatchDir hDir fp callback k -> runWatcherINotifyC (k inotify) _ --inotifyWatchDirectory hDir fp callback >> k
               in handleSum (eff . handle (inotify, ()) mergeResults) _ op
    
    
   -- WatcherINotifyC $ \inotify -> _
         
    
    --handleSum (eff . handle inotify) $ \(WatchDir hDir fp callback k) -> inotifyWatchDirectory hDir fp callback >> k

type INotifyImp fp sig m = (MonadIO m, Member (Reader INotify) sig, Member (Filesystem fp) sig, Carrier sig m) 


mergeResults :: Monad m => (INotify.INotify, WatcherINotifyC hDir fp m a) -> _
mergeResults (inotify, m) = do
  a <- runWatcherINotifyC m inotify
  return $ (inotify, m)

runINotify :: forall hDir fp sig m a. (Carrier sig m, MonadIO m, Member (Filesystem fp) sig)
           => Eff (WatcherINotifyC hDir fp m) a -> m a 
runINotify program = do
  inotify <- liftIO $ INotify.initINotify
  a <- runWatcherINotifyC (interpret program) inotify
  liftIO $ INotify.killINotify inotify
  return a

inotifyWatchDirectory :: INotifyImp fp sig m => hDir -> fp -> (FileEvent hDir fp -> IO ()) -> m INotify.WatchDescriptor
inotifyWatchDirectory hDir fp callback = do
  let watchTypes = [ INotify.Modify
                   , INotify.Attrib
                   , INotify.Move
                   , INotify.MoveOut
                   , INotify.Delete
                   , INotify.Create]
  inotify <- ask
  fp' <- toRawFilePath fp
  watchDescriptor <- liftIO $ INotify.addWatch inotify watchTypes fp' $
    handleEvent hDir fp callback 

  diskFiles <- listDirectory fp

  forM_ diskFiles $ \filename -> do
    filepath <- fp `appendPath` filename
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
  handler =
    case event of
      INotify.Created { isDirectory, filePath = filename } ->
        if isDirectory
          then callback $ CreatedDir hDir fp
          else callback $ CreatedFile hDir fp

      INotify.Modified { maybeFilePath } ->
        case maybeFilePath of
          Nothing -> return ()
          Just filename ->
            callback $ ModifiedFile hDir (error "how do defined this conversion?" filename)

      INotify.Deleted { filePath } -> return ()

      _ -> return ()

