
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}

module Effects.FileWatcher.INotify where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Exception (try, SomeException)

import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.Sum

import           Effects.Filesystem
import           Effects.FileWatcher

import           System.INotify                 ( INotify, isDirectory, filePath, maybeFilePath )
import qualified System.INotify                as INotify
import           Data.Coerce
import           Control.Concurrent
{-
instance (Carrier sig m, Effect sig) => Carrier (Teletype :+: sig) (TeletypeRetC m) where
  eff (L (Read    k)) = do
    i <- TeletypeRetC get
    case i of
      []  -> k ""
      h:t -> TeletypeRetC (put t) *> k h
  eff (L (Write s k)) = TeletypeRetC (tell [s]) *> k
eff (R other) = TeletypeRetC (eff (R (R (handleCoercible other))))

handle :: Functor f => f () -> (forall x. f (m x) -> n (f x)) -> sig m (m a) -> sig n (n (f a)) 

handleCoercible :: (HFunctor sig, Coercible f g) => sig f (f a) -> sig g (g a) 

eff :: sig h (h a) -> h a 

handleReader :: HFunctor sig => r -> (forall x. f x -> r -> g x) -> sig f (f a) -> sig g (g a) 
-}


newtype WatcherINotifyC hDir fp (m :: * -> *) (a :: *) = 
  WatcherINotifyC { runWatcherINotifyC :: INotify.INotify -> m a }
  deriving (Functor)

instance Applicative m => Applicative (WatcherINotifyC hDir fp (m :: * -> *)) where
  pure a = WatcherINotifyC $ \_ -> pure a
  a <*> b = WatcherINotifyC $ \i -> (runWatcherINotifyC a) i <*> (runWatcherINotifyC b) i

instance forall m fp sig hDir. (MonadIO m, Member (Filesystem fp) sig, Carrier sig m) 
         => Carrier (FileWatcher hDir fp :+: sig) (WatcherINotifyC hDir fp m) where
  ret a = WatcherINotifyC $ \_ -> return a
  eff = eff'

eff' :: forall m fp sig hDir a. (MonadIO m, Member (Filesystem fp) sig, Carrier sig m) 
      => (FileWatcher hDir fp :+: sig) (WatcherINotifyC hDir fp m) (WatcherINotifyC hDir fp m a) 
      -> WatcherINotifyC hDir fp m a
eff' (L (WatchDir dir fp callback k)) = (WatcherINotifyC $ \inotify -> inotifyWatchDirectory inotify dir fp callback) *> k 
eff' (R op) = WatcherINotifyC $ \inotify -> eff (handleReader inotify runWatcherINotifyC op)


type INotifyImp fp sig m = (MonadIO m, Member (Filesystem fp) sig, Carrier sig m) 

runINotify :: forall hDir fp sig m a.
           INotifyImp fp sig m
           => Eff (WatcherINotifyC hDir fp m) a -> m a 
runINotify program = do
  liftIO $ putStrLn "init inotify"
  inotify <- liftIO $ INotify.initINotify
  a <- runWatcherINotifyC (interpret program) inotify
  liftIO $ INotify.killINotify inotify
  liftIO $ putStrLn "finalize inotify"
  return a

inotifyWatchDirectory :: INotifyImp fp sig m
                      => INotify.INotify -> hDir -> fp -> (FileEvent hDir fp -> IO ()) -> m INotify.WatchDescriptor
inotifyWatchDirectory inotify hDir fp callback = do
  let watchTypes = [ INotify.Modify
                   , INotify.Attrib
                   , INotify.Move
                   , INotify.MoveOut
                   , INotify.Delete
                   , INotify.Create]
  fp' <- toRawFilePath fp
  liftIO $ print fp'
  watchDescriptor <- liftIO $ INotify.addWatch inotify watchTypes fp' $
    handleEvent hDir fp (\e -> void $ forkIO $ callback e)

  diskFiles <- listDirectory fp

  forM_ diskFiles $ \filename -> do
    filepath <- fp `appendPath` filename
    isDirectory <- doesDirectoryExist filepath
    if isDirectory
      then void $ inotifyWatchDirectory inotify (error "no sub hdir yet") filepath callback
      else liftIO $ callback $ CreatedFile hDir filepath
  
  return watchDescriptor

installPrintHandler :: IO () -> IO ()
installPrintHandler action = try action >>= \case
    Left  e -> liftIO $ print (e :: SomeException)
    Right _ -> return ()


handleEvent :: hDir -> fp -> (FileEvent hDir fp -> IO ()) -> INotify.Event -> IO ()
handleEvent hDir fp callback event = installPrintHandler $ do
    print event
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

      e -> liftIO $ print e

