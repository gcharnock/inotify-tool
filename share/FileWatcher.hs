module FileWatcher where

import           UnliftIO.Exception
import           Control.Monad.IO.Class
import qualified Data.ByteString.RawFilePath   as RFP
                                         hiding ( putStrLn )
import           Control.Monad
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           System.INotify hiding (withINotify)
import           Control.Monad.IO.Unlift
import           Control.Concurrent.STM
import           Data.Object
import qualified Data.Tree as Tree
import           ObjectStore
import           Logging.Contextual 
import           Logging.Contextual.BasicScheme 
import           Utils
import           RawFilePath.Directory         (RawFilePath)
import qualified RawFilePath.Directory         as RFP
import           Filesystem                    
import qualified System.Posix.ByteString       as Posix
import Control.Monad.Reader.Class
import Context

class HasINotify a where
  getINotify :: a -> INotify

class HasStateRoot a where
    getStateRoot :: a -> RawFilePath

class HasBroadcast a where
    getBroadcast :: a -> Maybe (TQueue Event)

class HasRootTree a where
    getRootTree :: a -> Tree.Tree

instance HasBroadcast Context where
    getBroadcast = cntxINotifyQueue


instance HasRootTree Context where
    getRootTree = getRootTree

instance HasStateRoot Context where
    getStateRoot =  getStateRoot

instance HasINotify Context where
    getINotify = getINotify 

broadcastEvent :: (MonadReader env m, MonadIO m, HasBroadcast env) => Event -> m ()
broadcastEvent event = do
    maybeQ <- fmap getBroadcast ask
    case maybeQ of 
        Just queue -> liftIO $ atomically $ writeTQueue queue event
        _ -> return ()

askRootTree :: (MonadReader env m, HasRootTree env) => m Tree.Tree
askRootTree = fmap getRootTree ask

askINotify :: (MonadReader env m, HasINotify env) => m INotify
askINotify = fmap getINotify ask

type FSWatcher env m = (MonadReader env m, MonadUnliftIO m, HasBroadcast env, HasLog env, HasRootTree env, HasStateRoot env, HasStore env, HasINotify env)


getCheckoutDir :: (MonadReader env m, HasStateRoot env) => m RawFilePath
getCheckoutDir = do
  stateRoot <- fmap getStateRoot ask
  return $ stateRoot </> "checkouts"

onNewFile :: FSWatcher env m => T.Text -> [RawFilePath] -> RawFilePath -> ObjectHash -> m ()
onNewFile checkoutName projectPath filename fileHash = do
  checkoutDir <- getCheckoutDir
  checkouts   <- liftIO $ RFP.listDirectory checkoutDir
  forM_ checkouts $ \checkout -> if T.decodeUtf8 checkout == checkoutName
    then return ()
    else do
      let dirname = checkoutDir </> checkout </> foldl (</>) "" projectPath
      file <- retrive fileHash
      let filepath = dirname </> filename
      [logInfo|REPLICATE: {filepath} {renderObjectHash fileHash}|]
      liftIO $ RFP.writeFile filepath file


handleEvent :: FSWatcher env m => T.Text -> [RawFilePath] -> Tree.Tree -> RawFilePath -> Event -> m ()
handleEvent checkoutName projectPath workingTree dirPath event =
  try handler >>= \case
    Left  e -> liftIO $ print (e :: SomeException)
    Right _ -> return ()
 where
  handler = do
    liftIO $ print event
    case event of
      Created { isDirectory, filePath = filename } -> do
        let filepath = dirPath </> filename
        if isDirectory
          then do
            newWorkingTree <- Tree.new
            storeDir workingTree newWorkingTree filepath
            startDirSync checkoutName newWorkingTree filepath []
          else do
            filehash <- storeFile workingTree filepath
            onNewFile checkoutName projectPath filename filehash
            return ()
        broadcastEvent event

      Modified { maybeFilePath } -> do
        case maybeFilePath of
          Nothing ->
            [logInfo|UNHANDLED: {maybeFilePath} was nothing|]
          Just filename -> do
            _ <- storeFile workingTree (dirPath </> filename)
            return ()
        broadcastEvent event

      Deleted { filePath } -> do
        [logInfo|REMOVE {filePath}|]
        Tree.remove workingTree filePath
        broadcastEvent event

      _ -> return ()

watchDirectory
  :: FSWatcher env m => T.Text -> [RawFilePath] -> Tree.Tree -> RawFilePath -> m WatchDescriptor
watchDirectory checkoutName projectPath workingTree filePath = do
  [logInfo|WATCH {filePath}|]
  iNotify <- askINotify
  withRunInIO $ \runInIO -> addWatch
    iNotify
    watchTypes
    filePath
    (\event -> runInIO
      $ handleEvent checkoutName projectPath workingTree filePath event
    )
  where watchTypes = [Modify, Attrib, Move, MoveOut, Delete, Create]

startDirSync :: FSWatcher env m => T.Text -> Tree.Tree -> RawFilePath -> [RawFilePath] -> m ()
startDirSync checkoutName workingTree thisDir projectPath = do
  [logInfo|SCAN: {thisDir}|]

  diskFiles <- liftIO $ RFP.listDirectory thisDir
  _         <- watchDirectory checkoutName projectPath workingTree thisDir

  forM_ diskFiles $ \filename -> do
    let filepath = thisDir </> filename
    isDirectory <- liftIO $ RFP.doesDirectoryExist filepath
    if not isDirectory
      then do
        fileHash <- storeFile workingTree filepath
        onNewFile checkoutName projectPath filename fileHash
      else do
        newTree <- Tree.new
        liftIO $ Tree.addSubTree workingTree filename newTree
        startDirSync checkoutName newTree filepath $ filename : projectPath

  Tree.forM_ workingTree $ \(filename, contents) -> do
    let filepath = thisDir </> filename
    case contents of
      Tree.ContentFile filehash -> do
        fileBytes <- retrive filehash
        liftIO $ RFP.writeFile filepath fileBytes
      _ -> return ()


startProjectSync :: (FSWatcher env m, HasRootTree env) => T.Text -> RawFilePath -> m ()
startProjectSync checkoutName thisDir = do
  rootTree <- askRootTree
  startDirSync checkoutName rootTree thisDir []


checkoutProject :: (FSWatcher env m, HasRootTree env) => T.Text -> RawFilePath -> m ()
checkoutProject checkoutName checkoutTo = do
  checkoutDir <- getCheckoutDir
  [logInfo|checkoutDir = {checkoutDir}|]
  let linkFilepath = checkoutDir </> T.encodeUtf8 checkoutName
  [logInfo|CHECKOUT: checkoutName: {linkFilepath} -> {checkoutTo}|]
  liftIO $ whenM (RFP.doesFileExist linkFilepath) $ error
    "project already checked out under that name"
  liftIO $ Posix.createSymbolicLink checkoutTo linkFilepath
  liftIO $ unlessM (RFP.doesFileExist checkoutTo) $ Posix.createDirectory
    checkoutTo
    Posix.ownerModes
  startProjectSync checkoutName linkFilepath