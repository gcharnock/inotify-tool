{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
module Logger where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Rainbow
import Rainbow ((&))

class Loggable a where
    toText :: a -> T.Text

instance Loggable T.Text where
    toText = id

instance Loggable BS.ByteString where 
    toText = T.decodeUtf8

class HasLogger m where
    trace :: T.Text -> m ()
    info :: T.Text -> m ()
    trace' :: Loggable a => a -> m ()
    trace' = trace . toText
    info' :: Loggable a => a -> m ()
    info' = info . toText

newtype LoggerT m a = LoggerT ()

newtype RainbowLogger = StdoutLogger ()

newtype RainbowLoggerT m a = RainbowLoggerT (ReaderT RainbowLogger m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance forall m. MonadUnliftIO m => MonadUnliftIO (RainbowLoggerT m) where
    askUnliftIO = do
        UnliftIO { unliftIO } <- RainbowLoggerT askUnliftIO
        return $ UnliftIO $ \(RainbowLoggerT a) -> unliftIO a
    withRunInIO f = RainbowLoggerT $ withRunInIO $ \g -> f $ \(RainbowLoggerT readerT) -> g readerT
        

instance MonadIO m => HasLogger (RainbowLoggerT m) where
    trace msg = liftIO $ Rainbow.putChunkLn $ Rainbow.chunk msg &
         Rainbow.fore Rainbow.blue &
         Rainbow.bold
    info msg = liftIO $ Rainbow.putChunkLn $ Rainbow.chunk msg &
         Rainbow.fore Rainbow.grey

runLoggerT :: Monad m => RainbowLoggerT m a -> m a
runLoggerT (RainbowLoggerT action) = runReaderT action $ StdoutLogger ()

inner :: (Monad m, HasLogger m) => m ()
inner = do
  trace "this is a trace"
  info "this is an info"

example :: IO ()
example = runLoggerT inner
