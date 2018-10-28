module Main where

import Data.Monoid
import           Control.Monad.IO.Class
import           Test.Hspec
import qualified Data.ByteString               as BS
import qualified Data.ByteString.RawFilePath as RFP
import           System.Posix.Temp.ByteString

import qualified Data.HashTable.IO             as H
import           Control.Monad.Trans.Reader
import           System.INotify
import           Whd                     hiding ( main )
import           LibWormhole


runDefaultApp :: MonadIO m => App a -> m a
runDefaultApp action = liftIO $ do
  objectStore <- H.new
  rootTree    <- fmap Tree H.new
  withINotify $ \inotify -> runReaderT
    action
    (Context
      { cntxINotify     = inotify
      , cntxObjectStore = objectStore
      , cntxRootTree    = rootTree
      }
    )

main :: IO ()
main = hspec $ do
  describe "filepath utils" $ do
    it "path seperator should be correct" $ pathSeperator `shouldBe` BS.head "/"


  describe "splitFilePath" $ it "should correctly split a path" $ do
    let (directoryPart, filePart) = splitFilepath "/simple/hello"
    directoryPart `shouldBe` "/simple"
    filePart `shouldBe` "hello"

  describe "startSyncProject" $ do
    it "should not add any objects when presented with an empty directory" $ do
        dirpath <- mkdtemp "/tmp/inotify-tool-testdir"
        len     <- runDefaultApp $ do
          rootTree <- getRootTree
          startProjectSync rootTree dirpath
          liftIO $ fmap length $ H.toList $ unTree rootTree :: App Int
        len `shouldBe` 0
    
    it "should add a file when presented with a directory with a single file in it" $ do
      dirpath <- mkdtemp "intify-tool-testdir"
      RFP.writeFile (dirpath <> "/hello.txt") "hello world"
      outTree <- runDefaultApp $ do
        rootTree <- getRootTree
        startProjectSync rootTree dirpath
        return rootTree
      let outTable = unTree outTree 
      outFile <- liftIO $ H.lookup outTable "hello.txt" 
      outFile `shouldSatisfy` (\case Just _ -> True; Nothing -> False)

