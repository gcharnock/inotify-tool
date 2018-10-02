module Main where

import System.FilePath
import Data.String.Interpolate.IsString
import Control.Monad
import System.INotify
import qualified System.Directory as Dir
import qualified Data.Text as T
import qualified Data.Text.IO as T

performInitialDirectorySweep :: FilePath -> IO ()
performInitialDirectorySweep thisDir = do
   T.putStrLn $ "scanning" <> (T.pack thisDir) <> ""
   files <- Dir.listDirectory thisDir
   forM_ files $ \file ->
     Dir.doesDirectoryExist file >>= \case
      False -> T.putStrLn [i|regular file #{file}|]
      True -> do
        T.putStrLn [i|directory #{file}|]
        performInitialDirectorySweep (thisDir </> file)


watch :: IO ()
watch = withINotify $ \inotify -> do
  performInitialDirectorySweep "."
  return ()
    
main :: IO ()
main = watch
