module Main where

import qualified System.FilePath as FP
import RawFilePath
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.RawFilePath as RBS
import RawFilePath
import qualified Data.ByteString as BS
import Data.String.Interpolate.IsString
import Control.Monad
import System.INotify
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Concurrent
import System.INotify
import Crypto.Hash
import Crypto.Hash.Algorithms
import Data.ByteArray.Encoding (convertToBase, Base (Base16))

infixr 5 </>

(</>) :: RawFilePath -> RawFilePath -> RawFilePath
a </> b = UTF8.fromString $ (UTF8.toString a) FP.</> (UTF8.toString b)

sha256File :: RawFilePath -> IO BS.ByteString
sha256File filepath = do
  fileBytes <- RBS.readFile filepath
  let digest = hashWith SHA256 fileBytes
      digest16 = convertToBase Base16 digest
  return digest16

handleEvent :: INotify -> Event -> IO ()
handleEvent inotify event = do
  print event
  case event of
    event@Created {isDirectory, filePath} -> do
      if isDirectory
        then do
          watchDirectory inotify filePath
          performInitialDirectorySweep inotify filePath
        else
          void $ watchRegularFile inotify filePath
      print event
    event -> print event

watchRegularFile :: INotify -> RawFilePath -> IO WatchDescriptor
watchRegularFile inotify filePath = do
    fileHash <- sha256File filePath
    T.putStrLn [i|watching regular file #{filePath} - #{fileHash}|]
    addWatch inotify watchTypes filePath (handleEvent inotify)
  where watchTypes = [ Modify, Attrib, Move, MoveOut, Delete ] 

watchDirectory :: INotify -> RawFilePath -> IO WatchDescriptor
watchDirectory inotify filePath = do
    T.putStrLn [i|watching directory #{filePath}|]
    addWatch inotify watchTypes filePath (handleEvent inotify)
  where watchTypes = [ Modify, Attrib, Move, MoveOut, Delete, Create, Delete ]

performInitialDirectorySweep :: INotify -> RawFilePath -> IO ()
performInitialDirectorySweep inotify thisDir = do
   BS.putStrLn $ "scanning " <> thisDir <> ""
   files <- listDirectory thisDir
   forM_ files $ \file -> do
     let filepath = (thisDir </> file)
     doesDirectoryExist filepath >>= \case
      False -> do
        void $ watchRegularFile inotify filepath
      True -> do
        watchDirectory inotify filepath
        performInitialDirectorySweep inotify filepath


watch :: IO ()
watch = withINotify $ \inotify -> do
  performInitialDirectorySweep inotify "."
  threadDelay $ 1000 * 1000 * 1000 * 1000
  return ()
    
main :: IO ()
main = watch
