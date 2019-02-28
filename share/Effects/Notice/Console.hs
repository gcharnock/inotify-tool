{-# LANGUAGE UndecidableInstances #-}
module Effects.Notice.Console where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum
import Control.Monad.IO.Class

import Effects.Notice

import qualified Data.Text.IO as T

newtype ConsoleNotice m a = ConsoleNotice { runConsoleNotice :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (MonadIO m, Carrier sig m) => Carrier (Notice :+: sig) (ConsoleNotice m) where
  ret = pure
  eff = handleSum (ConsoleNotice . eff . handleCoercible) $ cases 
      where cases = \case
              Notice msg k -> liftIO (T.putStrLn msg) >> k
