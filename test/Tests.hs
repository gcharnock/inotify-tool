module Main where

import           Control.Monad.Trans.Class
import           Data.Monoid
import           Control.Monad.IO.Class
import           Test.Hspec
import qualified Data.ByteString               as BS
                                         hiding ( putStrLn )
import qualified Data.ByteString.Char8         as BS
                                                ( putStrLn )
import qualified Data.ByteString.RawFilePath   as RFP
import           System.Posix.ByteString.FilePath
import           System.Posix.Directory.ByteString
import           System.Posix.Files.ByteString
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
  stateRoot <- mkdtemp "testdir/stateroot"
  withINotify $ \inotify -> runReaderT
    action
    (Context
      { cntxINotify     = inotify
      , cntxObjectStore = objectStore
      , cntxRootTree    = rootTree
      , cntxStateRoot   = stateRoot
      }
    )

data TestContext = TestContext {
  testCntxTmpDirA :: RawFilePath,
  testCntxTmpDirB :: RawFilePath
}

type TestEnv m = ReaderT TestContext m

withTestEnv :: TestEnv App a -> IO a
withTestEnv action = do
  tmpDirA <- mkdtemp "testdir/test_a"
  tmpDirB <- mkdtemp "testdir/test_b"
  BS.putStrLn $ "testdirs where A=" <> tmpDirA <> ", B=" <> tmpDirB
  runDefaultApp $ runReaderT
    action
    TestContext {testCntxTmpDirA = tmpDirA, testCntxTmpDirB = tmpDirB}

getTmpDirA :: Monad m => TestEnv m RawFilePath
getTmpDirA = fmap testCntxTmpDirA ask

getTmpDirB :: Monad m => TestEnv m RawFilePath
getTmpDirB = fmap testCntxTmpDirB ask

writeFileTestEnv :: MonadIO m => RawFilePath -> BS.ByteString -> TestEnv m ()
writeFileTestEnv filepath contents = do
  dirPath <- getTmpDirA
  liftIO $ RFP.writeFile (dirPath <> "/" <> filepath) contents

deleteFileTestEnv :: MonadIO m => RawFilePath -> TestEnv m ()
deleteFileTestEnv filepath = do
  dirPath <- getTmpDirA
  liftIO $ removeLink $ dirPath <> "/" <> filepath

mkDirTestEnv :: MonadIO m => RawFilePath -> TestEnv m ()
mkDirTestEnv filepath = do
  dirPath <- getTmpDirA
  liftIO $ createDirectory (dirPath <> "/" <> filepath) ownerModes


inApp :: App a -> TestEnv App a
inApp = lift

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "filepath utils"
    $          it "path seperator should be correct"
    $          pathSeperator
    `shouldBe` BS.head "/"


  describe "splitFilePath" $ it "should correctly split a path" $ do
    let (directoryPart, filePart) = splitFilepath "/simple/hello"
    directoryPart `shouldBe` "/simple"
    filePart `shouldBe` "hello"

  describe "startSyncProject" $ do
    it "should not add any objects when presented with an empty directory"
      $ withTestEnv
      $ do
          dirPath <- getTmpDirA
          inApp $ startProjectSync dirPath
          rootTree <- inApp getRootTree

          len      <- liftIO $ fmap length $ H.toList $ unTree rootTree
          liftIO $ len `shouldBe` 0

    it
        "should add a file when presented with a directory with a single file in it"
      $ withTestEnv
      $ do
          writeFileTestEnv "hello.txt" "hello world"

          dirPath <- getTmpDirA
          inApp $ startProjectSync dirPath

          outTable <- fmap unTree $ inApp getRootTree
          outFile  <- liftIO $ H.lookup outTable "hello.txt"
          liftIO $ outFile `shouldSatisfy` \case
            Just (ContentFile _) -> True
            _                    -> False


    it "should remove a file when the file is deleted" $ withTestEnv $ do
      writeFileTestEnv "hello.txt" "hello world"
      dirPath <- getTmpDirA
      inApp $ startProjectSync dirPath
      deleteFileTestEnv "hello.txt"

      waitForApp

      rootTree <- inApp getRootTree
      len      <- liftIO $ fmap length $ H.toList $ unTree rootTree
      liftIO $ len `shouldBe` 0


    it "should detect a new file created after the directory is watched"
      $ withTestEnv
      $ do
          dirPath <- getTmpDirA
          inApp $ startProjectSync dirPath

          writeFileTestEnv "hello.txt" "hello world"
          waitForApp

          outTable <- fmap unTree $ inApp getRootTree
          outFile  <- liftIO $ H.lookup outTable "hello.txt"
          liftIO $ outFile `shouldSatisfy` \case
            Just (ContentFile _) -> True
            _                    -> False

    it "should add a directory on initial scan" $ withTestEnv $ do
      dirPath <- getTmpDirA
      mkDirTestEnv "testDir"
      inApp $ startProjectSync dirPath
      outTable <- fmap unTree $ inApp getRootTree
      outDir   <- liftIO $ H.lookup outTable "testDir"
      liftIO $ outDir `shouldSatisfy` \case
        Just (ContentTree _) -> True
        _                    -> False


    it "should directory when one appears" $ withTestEnv $ do
      dirPath <- getTmpDirA
      inApp $ startProjectSync dirPath

      mkDirTestEnv "testDir"
      waitForApp

      outTable <- fmap unTree $ inApp getRootTree
      outDir   <- liftIO $ H.lookup outTable "testDir"
      liftIO $ outDir `shouldSatisfy` \case
        Just (ContentTree _) -> True
        _                    -> False

  describe "two target directories" $ do 
    it "calling startProjectSync on trees" $
      'a' `shouldBe` 'b'

    it "" $ withTestEnv $ do
      dirPathA <- getTmpDirA
      dirPathB <- getTmpDirB

      writeFileTestEnv "hello.txt" "content"

      inApp $ startProjectSync dirPathA
      inApp $ startProjectSync dirPathB

      outFile <- liftIO $ RFP.readFile $ dirPathB <> "/" <> "hello.txt"
      liftIO $ outFile `shouldBe` "content"

  describe "dumpToDirectory"
    $ it "should output a copy of a previously watched directory structure"
    $ withTestEnv
    $ do
        dirPath <- getTmpDirA
        writeFileTestEnv "hello.txt" "hello dump"
        mkDirTestEnv "subdir"
        writeFileTestEnv "subdir/subfile" "subfile contents"
        inApp $ startProjectSync dirPath

        outDir <- liftIO $ mkdtemp "testdir/dumpToDirectory"
        inApp $ getRootTree >>= \tree -> dumpToDirectory tree outDir

        hello <- liftIO $ RFP.readFile $ outDir <> "/" <> "hello.txt"
        liftIO $ hello `shouldBe` "hello dump"

        subfile <- liftIO $ RFP.readFile $ outDir <> "/" <> "subdir/subfile"
        liftIO $ subfile `shouldBe` "subfile contents"

