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
import qualified RawFilePath.Directory   as RFP

import qualified System.Posix.ByteString as Posix

import qualified Data.HashTable.IO             as H
import           Control.Monad.Trans.Reader
import           System.INotify
import           Whd                     hiding ( main )
import           LibWormhole
import           Control.Concurrent.STM


waitForApp :: TestEnv App ()
waitForApp = do
  TestContext { testCntxQueue } <- ask
  _ <- liftIO $ atomically $ readTQueue testCntxQueue
  return ()

runDefaultApp :: MonadIO m => TestContext -> App a -> m a
runDefaultApp testContext action = liftIO $ do
  objectStore <- H.new
  rootTree    <- fmap Tree H.new
  stateRoot   <- Posix.mkdtemp "testdir/stateroot"
  withINotify $ \inotify -> runReaderT
    action
    (Context
      { cntxINotify      = inotify
      , cntxObjectStore  = objectStore
      , cntxRootTree     = rootTree
      , cntxStateRoot    = stateRoot
      , cntxINotifyQueue = Just $ testCntxQueue testContext
      }
    )

data TestContext = TestContext {
  testCntxStateRoot :: Posix.RawFilePath,
  testCntxTmpDirA :: Posix.RawFilePath,
  testCntxTmpDirB :: Posix.RawFilePath,
  testCntxQueue :: TQueue Event
}

type TestEnv m = ReaderT TestContext m

withTestEnv :: TestEnv App a -> IO a
withTestEnv action = do
  cwd <- Posix.getWorkingDirectory
  tmpDirA   <- Posix.mkdtemp $ cwd </> "testdir/test_a_"
  cwd <- Posix.getWorkingDirectory
  RFP.removeDirectoryRecursive $ cwd </> "testdir"
  Posix.createDirectory $ cwd </> "testDir"
  tmpDirB   <- Posix.mkdtemp $ cwd </> "testdir/test_b_"
  stateRoot <- Posix.mkdtemp $ cwd </> "testdir/stateroot_"
  BS.putStrLn $ "testdirs where A=" <> tmpDirA <> ", B=" <> tmpDirB <> ",testRoot=" <> stateRoot
  queue <- atomically newTQueue
  let testContext = TestContext
        { testCntxTmpDirA   = tmpDirA
        , testCntxTmpDirB   = tmpDirB
        , testCntxQueue     = queue
        , testCntxStateRoot = stateRoot
        }
  out <- runDefaultApp testContext $ runReaderT action testContext
  RFP.removeDirectoryRecursive tmpDirA
  RFP.removeDirectoryRecursive tmpDirB
  RFP.removeDirectoryRecursive stateRoot
  return out

getTmpDirA :: Monad m => TestEnv m Posix.RawFilePath
getTmpDirA = fmap testCntxTmpDirA ask

getTmpDirB :: Monad m => TestEnv m Posix.RawFilePath
getTmpDirB = fmap testCntxTmpDirB ask

getStateDir :: Monad m => TestEnv m Posix.RawFilePath
getStateDir = fmap testCntxStateRoot ask

writeFileTestEnv :: MonadIO m => Posix.RawFilePath -> BS.ByteString -> TestEnv m ()
writeFileTestEnv filepath contents = do
  dirPath <- getTmpDirA
  liftIO $ RFP.writeFile (dirPath <> "/" <> filepath) contents

deleteFileTestEnv :: MonadIO m => Posix.RawFilePath -> TestEnv m ()
deleteFileTestEnv filepath = do
  dirPath <- getTmpDirA
  liftIO $ Posix.removeLink $ dirPath <> "/" <> filepath

mkDirTestEnv :: MonadIO m => Posix.RawFilePath -> TestEnv m ()
mkDirTestEnv filepath = do
  dirPath <- getTmpDirA
  liftIO $ Posix.createDirectory (dirPath <> "/" <> filepath) Posix.ownerModes


inApp :: App a -> TestEnv App a
inApp = lift

main :: IO ()
main = do 
  cwd <- Posix.getWorkingDirectory
  RFP.removeDirectoryRecursive $ cwd </> "testdir"
  Posix.createDirectory $ cwd </> "testDir"
  hspec spec

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

  describe "checkoutProject" $
    it "checkoutProject creates a link in the state dir" $ withTestEnv $ do
      dirPathA <- getTmpDirA
      stateDir <- getStateDir
      inApp $ checkoutProject "checkoutName" dirPathA

      linksTo <- liftIO $ Posix.readSymbolicLink $ stateDir </> "checkouts" </> "checkoutName"
      liftIO $ linksTo `shouldBe` dirPathA

  --  it "" $ withTestEnv $ do
  --    dirPathA <- getTmpDirA
  --    dirPathB <- getTmpDirB

  --    writeFileTestEnv "hello.txt" "content"

  --    inApp $ startProjectSync dirPathA
  --    inApp $ startProjectSync dirPathB

  --    outFile <- liftIO $ RFP.readFile $ dirPathB <> "/" <> "hello.txt"
  --    liftIO $ outFile `shouldBe` "content"

  --describe "dumpToDirectory"
  --  $ it "should output a copy of a previously watched directory structure"
  --  $ withTestEnv
  --  $ do
  --      dirPath <- getTmpDirA
  --      writeFileTestEnv "hello.txt" "hello dump"
  --      mkDirTestEnv "subdir"
  --      writeFileTestEnv "subdir/subfile" "subfile contents"
  --      inApp $ startProjectSync dirPath

  --      outDir <- liftIO $ Posix.mkdtemp "testdir/dumpToDirectory"
  --      inApp $ getRootTree >>= \tree -> dumpToDirectory tree outDir

  --      hello <- liftIO $ RFP.readFile $ outDir <> "/" <> "hello.txt"
  --      liftIO $ hello `shouldBe` "hello dump"

  --      subfile <- liftIO $ RFP.readFile $ outDir <> "/" <> "subdir/subfile"
  --      liftIO $ subfile `shouldBe` "subfile contents"

