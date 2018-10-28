module Main where

import Control.Monad.Trans.Class
import Data.Monoid
import           Control.Monad.IO.Class
import           Test.Hspec
import qualified Data.ByteString               as BS
import qualified Data.ByteString.RawFilePath as RFP
import System.Posix.ByteString.FilePath
import           System.Posix.Temp.ByteString

import qualified Data.HashTable.IO             as H
import           Control.Monad.Trans.Reader
import           System.INotify
import           Whd                     hiding ( main )
import           LibWormhole
import Control.Concurrent

waitForApp :: MonadIO m => m ()
waitForApp = liftIO $ threadDelay 500000

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

data TestContext = TestContext {
  testCntxTmpDir :: RawFilePath
}

type TestEnv m = ReaderT TestContext m

withTestEnv :: TestEnv App a -> IO a
withTestEnv action = do
  tmpDir <- mkdtemp "/tmp/inotify-tool-testdir"
  runDefaultApp $ do
    flip runReaderT TestContext { testCntxTmpDir = tmpDir } $ do
      action

getTmpDir :: Monad m => TestEnv m RawFilePath
getTmpDir = fmap testCntxTmpDir ask

writeFileTestEnv :: MonadIO m => RawFilePath -> BS.ByteString -> TestEnv m ()
writeFileTestEnv filepath contents = do
  dirPath <- getTmpDir
  liftIO $ RFP.writeFile (dirPath <> "/" <> filepath) contents

inApp :: App a -> TestEnv App a
inApp = lift

main :: IO ()
main = hspec $ do
  describe "filepath utils" $ do
    it "path seperator should be correct" $ pathSeperator `shouldBe` BS.head "/"


  describe "splitFilePath" $ it "should correctly split a path" $ do
    let (directoryPart, filePart) = splitFilepath "/simple/hello"
    directoryPart `shouldBe` "/simple"
    filePart `shouldBe` "hello"

  describe "startSyncProject" $ do
    it "should not add any objects when presented with an empty directory" $
      withTestEnv $ do
        dirPath <- getTmpDir
        inApp $ startProjectSync dirPath
        rootTree <- inApp getRootTree

        len <- liftIO $ fmap length $ H.toList $ unTree rootTree
        liftIO $ len `shouldBe` 0
    
    it "should add a file when presented with a directory with a single file in it" $ do
      withTestEnv $ do
        writeFileTestEnv "hello.txt" "hello world"

        dirPath <- getTmpDir 
        inApp $ startProjectSync dirPath

        outTable <- fmap unTree $ inApp getRootTree 
        outFile <- liftIO $ H.lookup outTable "hello.txt" 
        liftIO $ outFile `shouldSatisfy` (\case Just _ -> True; Nothing -> False)

    it "should detect a new file created after the directory is watched" $ do
      withTestEnv $ do
        dirPath <- getTmpDir
        inApp $ startProjectSync dirPath

        writeFileTestEnv "hello.txt" "hello world"
        waitForApp
        
        outTable <- fmap unTree $ inApp getRootTree 
        outFile <- liftIO $ H.lookup outTable "hello.txt" 
        liftIO $ outFile `shouldSatisfy` (\case Just _ -> True; Nothing -> False)

