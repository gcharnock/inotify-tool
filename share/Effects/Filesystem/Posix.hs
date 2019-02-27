{-# LANGUAGE UndecidableInstances #-}
module Effects.Filesystem.Posix where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum
import Control.Monad.IO.Class

import Effects.Filesystem

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified System.IO as Sys
import qualified System.Posix.Directory.ByteString as Posix
import qualified System.Posix.Files.ByteString as Posix
import qualified Data.ByteString.RawFilePath as RFP
import qualified RawFilePath.Directory as RFP

newtype FsPosix m a = FsPosix { runFsPosix :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)


instance (MonadIO m, Carrier sig m) => Carrier (Filesystem RFP.RawFilePath :+: sig) (FsPosix m) where
  ret = pure
  eff = handleSum (FsPosix . eff . handleCoercible) $ cases 
      where cases = \case
              OpenFile ioMode fp k -> liftIO (Sys.openFile (T.unpack $ T.decodeUtf8 fp) ioMode) >>= k
              CloseFile h k -> liftIO (Sys.hClose h) >> k
              ReadFile fp k -> liftIO (RFP.readFile fp) >>= k
              WriteFile fp contents k -> liftIO (RFP.writeFile fp contents) >> k
              CreateDirectory fp k -> liftIO (Posix.createDirectory fp Posix.ownerModes) >> k
              RemoveDirectory fp k -> liftIO (Posix.removeDirectory fp) >> k
              DeleteFile fp k -> liftIO (RFP.tryRemoveFile fp) >> k
              GetCWD k -> liftIO Posix.getWorkingDirectory >>= k
              LiftDirectory fp k -> LiftIO RFP.listDirectory >>= k
              AppendPath fp1 fp2 -> return $  UTF8.fromString $ (UTF8.toString fp1) FP.</> (UTF8.toString fp2)

