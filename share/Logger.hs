{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RoleAnnotations #-}
module Logger where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
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

type RainbowLoggerT m = ReaderT RainbowLogger m

instance MonadIO m => HasLogger (RainbowLoggerT m) where
    trace msg = liftIO $ Rainbow.putChunkLn $ Rainbow.chunk msg &
         Rainbow.fore Rainbow.blue &
         Rainbow.bold
    info msg = liftIO $ Rainbow.putChunkLn $ Rainbow.chunk msg &
         Rainbow.fore Rainbow.grey

instance (Monad m, HasLogger m, s ! RainbowLoggerT) => HasLogger (ReaderT s m) where
    trace = lift . trace
    info = lift . info
    info' = lift . info'
    trace' = lift .trace'

runLoggerT :: Monad m => RainbowLoggerT m a -> m a
runLoggerT action = runReaderT action $ StdoutLogger ()

inner :: (Monad m, HasLogger m) => m ()
inner = do
  trace "this is a trace"
  info "this is an info"

example :: IO ()
example = runLoggerT inner
