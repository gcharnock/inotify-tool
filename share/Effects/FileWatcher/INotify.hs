
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}

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
import Data.Proxy
import Data.Coerce
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


handleCoercible' :: (HFunctor sig, Coercible (WatcherINotifyC hDir fp m) m) =>
                          sig (WatcherINotifyC hDir fp m) (WatcherINotifyC hDir fp m a)
                          -> sig m (m a)
handleCoercible' = handleCoercible

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
eff' (R op) = WatcherINotifyC $ \inotify -> eff'' (handle' inotify op)
                where 
                  handle' :: forall a. INotify.INotify ->
                      sig (WatcherINotifyC hDir fp m) (WatcherINotifyC hDir fp m a)
                      -> sig m (m a)
                  handle' inotify = handleReader inotify runWatcherINotifyC
                  eff'' :: sig m (m a)
                          -> m a
                  eff'' = eff 


-- eff' :: (MonadIO m, Member (Filesystem fp) sig, Carrier sig m) 
--      => (FileWatcher hDir fp :+: sig) (WatcherINotifyC hDir fp m) (WatcherINotifyC hDir fp m a) 
--      -> WatcherINotifyC hDir fp m a
-- eff' op = handleSum left right op
-- 
-- left :: forall sig hDir fp (m :: * -> *) (a :: *).
--      sig (WatcherINotifyC hDir fp m) (WatcherINotifyC hDir fp m a)
--      -> WatcherINotifyC hDir fp m a
-- left op =
--   let h = handleCoercible :: (HFunctor sig)
--                           => (forall (x :: *). (WatcherINotifyC hDir fp m x) -> m x)
--                           -> sig (WatcherINotifyC hDir fp m) (WatcherINotifyC hDir fp m a)
--                           -> sig m (m a)
--   in WatcherINotifyC $ \_ -> _
-- 
-- right :: FileWatcher hDir fp (WatcherINotifyC hDir fp m) (WatcherINotifyC hDir fp m a)
--       -> WatcherINotifyC hDir fp m a
-- right op = _

    
   {- WatcherINotifyC $ \inotify ->
               let cases = \case
                      WatchDir hDir fp callback k -> runWatcherINotifyC (k inotify) _ --inotifyWatchDirectory hDir fp callback >> k
               in handleSum (eff . handle (inotify, ()) mergeResults) _ op-}
    
    
   -- WatcherINotifyC $ \inotify -> _
         
    
    --handleSum (eff . handle inotify) $ \(WatchDir hDir fp callback k) -> inotifyWatchDirectory hDir fp callback >> k

type INotifyImp fp sig m = (MonadIO m, Member (Filesystem fp) sig, Carrier sig m) 


-- mergeResults :: Monad m => (INotify.INotify, WatcherINotifyC hDir fp m a) -> _
-- mergeResults (inotify, m) = do
--   a <- runWatcherINotifyC m inotify
--   return $ (inotify, m)

runINotify :: forall hDir fp sig m a. (Carrier sig m, MonadIO m, Member (Filesystem fp) sig)
           => Eff (WatcherINotifyC hDir fp m) a -> m a 
runINotify program = do
  inotify <- liftIO $ INotify.initINotify
  a <- runWatcherINotifyC (interpret program) inotify
  liftIO $ INotify.killINotify inotify
  return a

inotifyWatchDirectory :: INotifyImp fp sig m => INotify.INotify -> hDir -> fp -> (FileEvent hDir fp -> IO ()) -> m INotify.WatchDescriptor
inotifyWatchDirectory inotify hDir fp callback = do
  let watchTypes = [ INotify.Modify
                   , INotify.Attrib
                   , INotify.Move
                   , INotify.MoveOut
                   , INotify.Delete
                   , INotify.Create]
  fp' <- toRawFilePath fp
  watchDescriptor <- liftIO $ INotify.addWatch inotify watchTypes fp' $
    handleEvent hDir fp callback 

  diskFiles <- listDirectory fp

  forM_ diskFiles $ \filename -> do
    filepath <- fp `appendPath` filename
    isDirectory <- doesDirectoryExist filepath
    if isDirectory
      then void $ inotifyWatchDirectory inotify (error "no sub hdir yet") filepath callback
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

