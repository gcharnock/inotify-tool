
module UnliftIO.INotify (
    initINotify,
    withINotify
) where

import qualified System.INotify as A
import UnliftIO
import Control.Monad.IO.Class

initINotify :: MonadIO m => m A.INotify
initINotify = liftIO A.initINotify

withINotify :: MonadUnliftIO m => (A.INotify -> m a) -> m a
withINotify action = withRunInIO $ \runInIO -> 
    A.withINotify $ \inotify -> (runInIO $ action inotify)


