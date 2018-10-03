module Main where

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as BS
import System.FilePath
import Data.String.Interpolate.IsString
import Control.Monad
import System.INotify
import qualified System.Directory as Dir
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Concurrent
import System.INotify


handleEvent :: INotify -> Event -> IO ()
handleEvent inotify = \case
  event@Created {isDirectory, filePath} -> do
    newDescriptor <- addWatch inotify watchTypes "." (handleEvent inotify)
    print event
  event -> print event

watchTypes :: [EventVariety]
watchTypes = [ Modify, Attrib, Move, MoveIn, MoveOut, Create, Delete ]


watchRegularFile :: INotify -> FilePath -> IO WatchDescriptor
watchRegularFile inotify filePath = do
    T.putStrLn [i|watching regular file #{filePath}|]
    addWatch inotify watchTypes filePathUTF8 (handleEvent inotify)
  where watchTypes = [ Modify, Attrib, Move, MoveOut, Delete ] 
        filePathUTF8 = UTF8.fromString filePath

watchDirectory :: INotify -> FilePath -> IO WatchDescriptor
watchDirectory inotify filePath = do
    T.putStrLn [i|watching directory #{filePath}|]
    addWatch inotify watchTypes filePathUTF8 (handleEvent inotify)
  where watchTypes = [ Modify, Attrib, Move, MoveOut, Delete, Create, Delete ]
        filePathUTF8 = UTF8.fromString filePath

performInitialDirectorySweep :: INotify -> FilePath -> IO ()
performInitialDirectorySweep inotify thisDir = do
   T.putStrLn $ "scanning" <> (T.pack thisDir) <> ""
   files <- Dir.listDirectory thisDir
   forM_ files $ \file -> do
     let filepath = (thisDir </> file)
     Dir.doesDirectoryExist file >>= \case
      False -> do
        void $ watchRegularFile inotify filepath
      True -> do
        watchDirectory inotify file
        performInitialDirectorySweep inotify filepath


watch :: IO ()
watch = withINotify $ \inotify -> do
  performInitialDirectorySweep inotify "."
  threadDelay $ 1000 * 1000 * 1000 * 1000
  return ()
    
main :: IO ()
main = watch
