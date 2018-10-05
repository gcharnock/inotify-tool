module Main where

import Control.Monad.IO.Class
import qualified System.FilePath               as FP
import qualified Data.ByteString.UTF8          as UTF8
import qualified Data.ByteString.RawFilePath   as RBS
import           RawFilePath
import qualified Data.ByteString               as BS
import           Data.String.Interpolate.IsString
import           Control.Monad
import           System.INotify
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Control.Concurrent
import           System.INotify
import           Crypto.Hash                    ( Digest
                                                , SHA256(..)
                                                , hashWith
                                                )
import           Data.ByteArray.Encoding        ( convertToBase
                                                , Base(..)
                                                )
import Control.Monad.Trans.Reader
import qualified Data.HashTable.IO             as H
import Control.Monad.IO.Unlift

type HashTable k v = H.BasicHashTable k v

data Context = Context {
  cntxINotify :: INotify,
  cntxObjectStore :: HashTable (Digest SHA256) BS.ByteString
}

type App a = ReaderT Context IO a


data Tree = Tree {
  treeFilename :: RawFilePath,
  treeSha256Hash :: HashTable RawFilePath (Digest SHA256)
} deriving (Show)

infixr 5 </>

(</>) :: RawFilePath -> RawFilePath -> RawFilePath
a </> b = UTF8.fromString $ (UTF8.toString a) FP.</> (UTF8.toString b)

renderSha256 :: Digest SHA256 -> BS.ByteString
renderSha256 = convertToBase Base16

sha256File :: MonadIO m => RawFilePath -> m (Digest SHA256)
sha256File filepath = do
  fileBytes <- liftIO $ RBS.readFile filepath
  return $ hashWith SHA256 fileBytes

handleEvent :: Event -> App ()
handleEvent event = do
  liftIO $ print event
  case event of
    event@Created { isDirectory, filePath } -> do
      if isDirectory
        then do
          watchDirectory filePath
          performInitialDirectorySweep filePath
        else void $ watchRegularFile filePath
      liftIO $ print event
    event -> liftIO $ print event

watchRegularFile :: RawFilePath -> App WatchDescriptor
watchRegularFile filePath = do
  fileHash <- liftIO $ sha256File filePath
  liftIO $ T.putStrLn [i|watching regular file #{filePath} - #{fileHash}|]
  Context { cntxINotify } <- ask
  withRunInIO $ \runInIO -> 
    addWatch cntxINotify watchTypes filePath (\event -> (runInIO $ handleEvent event))
  where watchTypes = [Modify, Attrib, Move, MoveOut, Delete]

watchDirectory :: RawFilePath -> App WatchDescriptor
watchDirectory filePath = do
  liftIO $ T.putStrLn [i|watching directory #{filePath}|]
  Context { cntxINotify } <- ask
  withRunInIO $ \ runInIO ->
    addWatch cntxINotify watchTypes filePath (\event -> runInIO $ handleEvent event)
  where watchTypes = [Modify, Attrib, Move, MoveOut, Delete, Create, Delete]

performInitialDirectorySweep :: RawFilePath -> App ()
performInitialDirectorySweep thisDir = do
  Context { cntxINotify = inotify } <- ask
  liftIO $ BS.putStrLn $ "scanning " <> thisDir <> ""
  files <- liftIO $ listDirectory thisDir
  forM_ files $ \file -> do
    let filepath = (thisDir </> file)
    liftIO (doesDirectoryExist filepath) >>= \case
      False -> do
        void $ watchRegularFile filepath
      True -> do
        watchDirectory filepath
        performInitialDirectorySweep filepath


watch :: App ()
watch = do
  performInitialDirectorySweep "."
  liftIO $ threadDelay $ 1000 * 1000 * 1000 * 1000
  return ()

main :: IO ()
main = do
  objectStore <- H.new
  withINotify $ \inotify -> do
    runReaderT watch (Context {
      cntxINotify = inotify,
      cntxObjectStore = objectStore
    })
