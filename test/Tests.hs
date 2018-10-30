module Main where

import           Control.Monad.Trans.Class
import           Data.Monoid
import           Control.Monad.IO.Class
import           Test.Hspec
import qualified Data.ByteString               as BS hiding (putStrLn)
import qualified Data.ByteString.Char8         as BS (putStrLn)
import qualified Data.ByteString.RawFilePath   as RFP
import           System.Posix.ByteString.FilePath
import           System.Posix.Directory.ByteString
import           System.Posix.Temp.ByteString

import qualified Data.HashTable.IO             as H
import           Control.Monad.Trans.Reader
import           System.INotify
import           Whd                     hiding ( main )
import           LibWormhole
import           Control.Concurrent

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
  tmpDir <- mkdtemp "testdir/test"
  BS.putStrLn $ "testdir was " <> tmpDir
  runDefaultApp $
    flip runReaderT TestContext {testCntxTmpDir = tmpDir} $
      action

getTmpDir :: Monad m => TestEnv m RawFilePath
getTmpDir = fmap testCntxTmpDir ask

writeFileTestEnv :: MonadIO m => RawFilePath -> BS.ByteString -> TestEnv m ()
writeFileTestEnv filepath contents = do
  dirPath <- getTmpDir
  liftIO $ RFP.writeFile (dirPath <> "/" <> filepath) contents

mkDirTestEnv :: MonadIO m => RawFilePath -> TestEnv m ()
mkDirTestEnv filepath = do
  dirPath <- getTmpDir
  liftIO $ createDirectory (dirPath <> "/" <> filepath) 0o700 


inApp :: App a -> TestEnv App a
inApp = lift

main :: IO ()
main = hspec spec 

spec :: Spec
spec = do
  describe "filepath utils" $
    it "path seperator should be correct" $ pathSeperator `shouldBe` BS.head "/"


  describe "splitFilePath" $ it "should correctly split a path" $ do
    let (directoryPart, filePart) = splitFilepath "/simple/hello"
    directoryPart `shouldBe` "/simple"
    filePart `shouldBe` "hello"

  describe "startSyncProject" $ do
    it "should not add any objects when presented with an empty directory"
      $ withTestEnv
      $ do
          dirPath <- getTmpDir
          inApp $ startProjectSync dirPath
          rootTree <- inApp getRootTree

          len      <- liftIO $ fmap length $ H.toList $ unTree rootTree
          liftIO $ len `shouldBe` 0

    it
        "should add a file when presented with a directory with a single file in it"
      $
          withTestEnv $ do
            writeFileTestEnv "hello.txt" "hello world"

            dirPath <- getTmpDir
            inApp $ startProjectSync dirPath

            outTable <- fmap unTree $ inApp getRootTree
            outFile  <- liftIO $ H.lookup outTable "hello.txt"
            liftIO
              $               outFile
              `shouldSatisfy` (\case
                                Just _  -> True
                                Nothing -> False
                              )

    it "should detect a new file created after the directory is watched" $
      withTestEnv $ do
        dirPath <- getTmpDir
        inApp $ startProjectSync dirPath

        writeFileTestEnv "hello.txt" "hello world"
        waitForApp

        outTable <- fmap unTree $ inApp getRootTree
        outFile  <- liftIO $ H.lookup outTable "hello.txt"
        liftIO
          $               outFile
          `shouldSatisfy` \case
                            Just (ContentFile _)  -> True
                            _ -> False

    it "should add a directory on initial scan" $
      withTestEnv $ do
        dirPath <- getTmpDir
        mkDirTestEnv "testDir"
        inApp $ startProjectSync dirPath
        outTable <- fmap unTree $ inApp getRootTree
        outDir   <- liftIO $ H.lookup outTable "testDir"
        liftIO
          $              outDir 
          `shouldSatisfy` \case
                            Just (ContentTree _)  -> True
                            _ -> False
                          

    it "should directory when one appears" $
      withTestEnv $ do
        dirPath <- getTmpDir
        inApp $ startProjectSync dirPath

        mkDirTestEnv "testDir"
        waitForApp

        outTable <- fmap unTree $ inApp getRootTree
        outDir   <- liftIO $ H.lookup outTable "testDir"
        liftIO
          $              outDir 
          `shouldSatisfy` \case
                            Just (ContentTree _)  -> True
                            _ -> False
