module Main where

import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class
import           Test.Hspec
import qualified Data.ByteString               as BS
                                         hiding ( putStrLn )
import qualified Data.ByteString.Char8         as BS
                                                ( putStrLn )
import qualified Data.ByteString.RawFilePath   as RFP
import qualified RawFilePath.Directory         as RFP

import qualified System.Posix.ByteString       as Posix

import           Control.Monad.Trans.Reader
import           System.INotify
import           Control.Concurrent.STM
import qualified Data.Text.Encoding            as T
import qualified Data.Text.IO                  as T

import           LibWormhole
import           Whd                     hiding ( main )
import Tree
import ObjectStore
import Filesystem
import Utils

waitForApp :: TestEnv App ()
waitForApp = do
  TestContext { testCntxQueue } <- ask
  _ <- liftIO $ atomically $ readTQueue testCntxQueue
  return ()

runDefaultApp :: MonadIO m => TestContext -> App a -> m a
runDefaultApp testContext action = liftIO $ do
  objectStore <- newInMemoryStore
  rootTree    <- newTree
  withINotify $ \inotify -> runReaderT
    action
    (Context
      { cntxINotify      = inotify
      , cntxObjectStore  = objectStore
      , cntxRootTree     = rootTree
      , cntxStateRoot    = testCntxStateRoot testContext
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
  T.putStrLn $ "cwd = " <> T.decodeUtf8 cwd
  tmpDirA   <- Posix.mkdtemp $ cwd </> "testdir/test_a_"
  tmpDirB   <- Posix.mkdtemp $ cwd </> "testdir/test_b_"
  stateRoot <- Posix.mkdtemp $ cwd </> "testdir/stateroot_"
  BS.putStrLn $  "DIR_A=" <> tmpDirA
  BS.putStrLn $  "DIR_B=" <> tmpDirB
  BS.putStrLn $  "STATE_DIR=" <> stateRoot

  queue <- atomically newTQueue
  let testContext = TestContext
        { testCntxTmpDirA   = tmpDirA
        , testCntxTmpDirB   = tmpDirB
        , testCntxQueue     = queue
        , testCntxStateRoot = stateRoot
        }
  out <- runDefaultApp testContext $ flip runReaderT testContext $ do
    inApp ensureStateDirIsSetup
    action
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

writeFileTestEnvA
  :: MonadIO m => Posix.RawFilePath -> BS.ByteString -> TestEnv m ()
writeFileTestEnvA filepath contents = do
  dirPath <- getTmpDirA
  liftIO $ RFP.writeFile (dirPath <> "/" <> filepath) contents

writeFileTestEnvB
  :: MonadIO m => Posix.RawFilePath -> BS.ByteString -> TestEnv m ()
writeFileTestEnvB filepath contents = do
  dirPath <- getTmpDirB
  liftIO $ RFP.writeFile (dirPath <> "/" <> filepath) contents

deleteFileTestEnv :: MonadIO m => Posix.RawFilePath -> TestEnv m ()
deleteFileTestEnv filepath = do
  dirPath <- getTmpDirA
  liftIO $ Posix.removeLink $ dirPath <> "/" <> filepath

mkDirTestEnvA :: MonadIO m => Posix.RawFilePath -> TestEnv m ()
mkDirTestEnvA filepath = do
  dirPath <- getTmpDirA
  liftIO $ Posix.createDirectory (dirPath <> "/" <> filepath) Posix.ownerModes

mkDirTestEnvB :: MonadIO m => Posix.RawFilePath -> TestEnv m ()
mkDirTestEnvB filepath = do
  dirPath <- getTmpDirA
  liftIO $ Posix.createDirectory (dirPath <> "/" <> filepath) Posix.ownerModes

inApp :: App a -> TestEnv App a
inApp = lift

main :: IO ()
main = hspec spec

setup :: IO ()
setup = do
  cwd <- Posix.getWorkingDirectory
  let tmpdir = cwd </> "testdir"
  whenM (Posix.fileExist tmpdir) $ RFP.removeDirectoryRecursive tmpdir
  Posix.createDirectory tmpdir Posix.ownerModes

spec :: Spec
spec = beforeAll_ setup $ do
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
          inApp $ startProjectSync "irrelevent" dirPath
          rootTree <- inApp getRootTree

          len      <- getTreeLen rootTree
          liftIO $ len `shouldBe` 0

    it
        "should add a file when presented with a directory with a single file in it"
      $ withTestEnv
      $ do
          writeFileTestEnvA "hello.txt" "hello world"

          dirPath <- getTmpDirA
          inApp $ startProjectSync "irrelevent" dirPath

          outTable <- inApp getRootTree
          outFile  <- treeLookup outTable "hello.txt"
          liftIO $ outFile `shouldSatisfy` \case
            Just (ContentFile _) -> True
            _                    -> False


    it "should remove a file when the file is deleted" $ withTestEnv $ do
      writeFileTestEnvA "hello.txt" "hello world"
      dirPath <- getTmpDirA
      inApp $ startProjectSync "irrelevent" dirPath
      deleteFileTestEnv "hello.txt"

      waitForApp

      rootTree <- inApp getRootTree
      len      <- getTreeLen rootTree
      liftIO $ len `shouldBe` 0


    it "should detect a new file created after the directory is watched"
      $ withTestEnv
      $ do
          dirPath <- getTmpDirA
          inApp $ startProjectSync "irrelevent" dirPath

          writeFileTestEnvA "hello.txt" "hello world"
          waitForApp

          rootTree <- inApp getRootTree
          outFile  <- treeLookup rootTree "hello.txt"
          liftIO $ outFile `shouldSatisfy` \case
            Just (ContentFile _) -> True
            _                    -> False

    it "should add a directory on initial scan" $ withTestEnv $ do
      dirPath <- getTmpDirA
      mkDirTestEnvA "testDir"
      inApp $ startProjectSync "irrelevent" dirPath
      rootTree <- inApp getRootTree
      outDir   <- treeLookup rootTree "testDir"
      liftIO $ outDir `shouldSatisfy` \case
        Just (ContentTree _) -> True
        _                    -> False


    it "should directory when one appears" $ withTestEnv $ do
      dirPath <- getTmpDirA
      inApp $ startProjectSync "irrelevent" dirPath

      mkDirTestEnvA "testDir"
      waitForApp

      rootTree <- inApp getRootTree
      outDir   <- treeLookup rootTree "testDir"
      liftIO $ outDir `shouldSatisfy` \case
        Just (ContentTree _) -> True
        _                    -> False

  describe "checkoutProject" $ do
    it "creates a link in the state dir" $ withTestEnv $ do
      dirPathA <- getTmpDirA
      stateDir <- getStateDir
      inApp $ checkoutProject "checkoutName" dirPathA

      linksTo <-
        liftIO
        $   Posix.readSymbolicLink
        $   stateDir
        </> "checkouts"
        </> "checkoutName"
      liftIO $ linksTo `shouldBe` dirPathA

    it
        "will clone files in an existing checkout into a second checkout"
      $ withTestEnv
      $ do
          dirPathA <- getTmpDirA
          dirPathB <- getTmpDirB
          writeFileTestEnvA "hello.txt" "hello world"
          inApp $ checkoutProject "checkoutA" dirPathA
          inApp $ checkoutProject "checkoutB" dirPathB

          liftIO
            $ unlessM (Posix.fileExist $ dirPathB </> "hello.txt")
            $ expectationFailure "hello.txt was not copied"

    it "will clone files in the second checkout back to the existing project"
      $ withTestEnv
      $ do
          dirPathA <- getTmpDirA
          dirPathB <- getTmpDirB
          writeFileTestEnvB "hello.txt" "hello world"
          inApp $ checkoutProject "checkoutA" dirPathA
          inApp $ checkoutProject "checkoutB" dirPathB

          liftIO
            $ unlessM (Posix.fileExist $ dirPathA </> "hello.txt")
            $ expectationFailure "hello.txt was not copied"

  describe "Live replication" $ do
    it "will clone a new file created in one checkout to all other checkouts"
      $ withTestEnv
      $ do
        --given
        dirPathA <- getTmpDirA
        dirPathB <- getTmpDirB
        inApp $ checkoutProject "checkoutA" dirPathA
        inApp $ checkoutProject "checkoutB" dirPathB

        --if 
        writeFileTestEnvB "hello.txt" "hello world"
        waitForApp

        --then
        liftIO
          $ unlessM (Posix.fileExist $ dirPathA </> "hello.txt")
          $ expectationFailure "hello.txt was not copied"

    it "will clone a new directory created in one checkout to all other checkouts"
      $ withTestEnv
      $ do
        --given
        dirPathA <- getTmpDirA
        dirPathB <- getTmpDirB
        inApp $ checkoutProject "checkoutA" dirPathA
        inApp $ checkoutProject "checkoutB" dirPathB

        --if 
        mkDirTestEnvA "testdir"
        waitForApp

        --then
        liftIO
          $ unlessM (RFP.doesDirectoryExist $ dirPathB </> "testdir")
          $ expectationFailure "directory was not copied"

  --  it "" $ withTestEnv $ do
  --    dirPathA <- getTmpDirA
  --    dirPathB <- getTmpDirB

  --    writeFileTestEnvA "hello.txt" "content"

  --    inApp $ startProjectSync dirPathA
  --    inApp $ startProjectSync dirPathB

  --    outFile <- liftIO $ RFP.readFile $ dirPathB <> "/" <> "hello.txt"
  --    liftIO $ outFile `shouldBe` "content"

  --describe "dumpToDirectory"
  --  $ it "should output a copy of a previously watched directory structure"
  --  $ withTestEnv
  --  $ do
  --      dirPath <- getTmpDirA
  --      writeFileTestEnvA "hello.txt" "hello dump"
  --      mkDirTestEnvA "subdir"
  --      writeFileTestEnvA "subdir/subfile" "subfile contents"
  --      inApp $ startProjectSync dirPath

  --      outDir <- liftIO $ Posix.mkdtemp "testdir/dumpToDirectory"
  --      inApp $ getRootTree >>= \tree -> dumpToDirectory tree outDir

  --      hello <- liftIO $ RFP.readFile $ outDir <> "/" <> "hello.txt"
  --      liftIO $ hello `shouldBe` "hello dump"

  --      subfile <- liftIO $ RFP.readFile $ outDir <> "/" <> "subdir/subfile"
  --      liftIO $ subfile `shouldBe` "subfile contents"

