{-# LANGUAGE UndecidableInstances #-}
module Effects.Filesystem.Posix where

import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.Sum
import           Control.Monad.IO.Class

import           Effects.Filesystem

import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T

import qualified System.IO                     as Sys
import qualified System.FilePath               as FP
import qualified System.Posix.Directory.ByteString
                                               as Posix
import qualified System.Posix.Files.ByteString as Posix
import qualified Data.ByteString.RawFilePath   as RFP
import qualified RawFilePath.Directory         as RFP
import qualified Data.ByteString.UTF8          as UTF8

newtype FsPosixC m a = FsPosixC { runFsPosixC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)


instance (MonadIO m, Carrier sig m) => Carrier (Filesystem RFP.RawFilePath :+: sig) (FsPosixC m) where
  ret = pure
  eff = handleSum (FsPosixC . eff . handleCoercible) $ cases
   where
    cases = \case
      OpenFile ioMode fp k ->
        liftIO (Sys.openFile (T.unpack $ T.decodeUtf8 fp) ioMode) >>= k
      CloseFile _ h k         -> liftIO (Sys.hClose h) >> k
      ReadFile fp k           -> liftIO (RFP.readFile fp) >>= k
      WriteFile fp contents k -> liftIO (RFP.writeFile fp contents) >> k
      CreateDirectory fp k ->
        liftIO (Posix.createDirectory fp Posix.ownerModes) >> k
      DoesDirectoryExist fp k -> liftIO (RFP.doesDirectoryExist fp) >>= k
      RemoveDirectory    fp k -> liftIO (Posix.removeDirectory fp) >> k
      DeleteFile         fp k -> liftIO (RFP.tryRemoveFile fp) >> k
      GetCWD k                -> liftIO Posix.getWorkingDirectory >>= k
      ListDirectory fp k      -> liftIO (RFP.listDirectory fp) >>= k
      AppendPath fp1 fp2 k ->
        k $ UTF8.fromString $ (UTF8.toString fp1) FP.</> (UTF8.toString fp2)
      ToRawFilePath   fp k -> k fp
      FromRawFilePath fp k -> k fp

runFsPosix :: (Carrier sig m, Effect sig, Monad m) => Eff (FsPosixC m) a -> m a
runFsPosix program = runFsPosixC (interpret program)
