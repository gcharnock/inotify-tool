{-# LANGUAGE FlexibleInstances #-}
module Whd where

import           Control.Monad.Trans            ( lift )
import qualified System.Directory              as Sys
import           Data.Monoid
import           System.IO
import           UnliftIO.Exception
import           UnliftIO.Async
import           Control.Monad.IO.Class
import qualified System.FilePath               as FP
import qualified Data.ByteString               as BS
                                         hiding ( hPutStrLn
                                                , putStrLn
                                                , unpack
                                                , pack
                                                )
import qualified Data.ByteString.UTF8          as UTF8
import qualified Data.ByteString.RawFilePath   as RFP
                                         hiding ( putStrLn )
import qualified Data.ByteString.Char8         as BS
import           Control.Monad
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.IO                  as T
import           Control.Concurrent
import           System.INotify
import           Crypto.Hash                    ( Digest
                                                , SHA256(..)
                                                , hashWith
                                                )
import           Data.ByteArray.Encoding        ( convertToBase
                                                , Base(..)
                                                )
import           Control.Monad.Trans.Reader
import qualified Data.HashTable.IO             as H
import           Control.Monad.IO.Unlift
import           Data.Hashable
import           Data.ByteArray
import           Network.Socket
import           Data.Aeson
import           LibWormhole
import qualified Pipes                         as P
import qualified Pipes.Binary                  as P
import qualified Pipes.ByteString              as P
import qualified Pipes.Parse                   as P
import qualified Data.Binary.Get               as Bin
import           Control.Concurrent.STM
import Object
import Tree
import ObjectStore (storeFile, storeDir, ObjectStore(..), TreeContent(..), HasStore, getStore)
import Logger
import Utils
import Filesystem
import qualified System.Posix.ByteString as Posix



data Context = Context {
  cntxINotify :: INotify,
  cntxObjectStore :: ObjectStore,
  cntxRootTree :: Tree,
  cntxStateRoot :: RawFilePath,
  cntxINotifyQueue :: Maybe (TQueue Event)
}


type App = ReaderT Context IO

getRootTree :: App Tree
getRootTree = fmap cntxRootTree ask

getStateRoot :: App RawFilePath
getStateRoot = fmap cntxStateRoot ask

getCheckoutDir :: App RawFilePath
getCheckoutDir = do
  stateRoot <- getStateRoot
  return $ stateRoot </> "checkouts"

instance Monad m => HasStore (ReaderT Context m) where
  getStore = asks cntxObjectStore

class Monad m => HasLogger (ReaderT Context m) where
    trace :: T.Text -> m ()
    info :: T.Text -> m ()

data UserReqContext = UserReqContext {
  ucntxHandle :: Handle
}

broadcastEvent :: Event -> App ()
broadcastEvent event = ask >>= \case
  Context { cntxINotifyQueue = Just queue } ->
    liftIO $ atomically $ writeTQueue queue event
  _ -> return ()

type UserRequest = ReaderT UserReqContext App

handleEvent :: T.Text -> [RawFilePath] -> Tree -> RawFilePath -> Event -> App ()
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
            newWorkingTree <- newTree 
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
            info $ "UNHANDLED: " <> showT maybeFilePath <> "was nothing"
          Just filename -> do
            _ <- storeFile workingTree (dirPath </> filename)
            return ()
        broadcastEvent event

      Deleted { filePath } -> do
        info $ "REMOVE: " <> T.decodeUtf8 filePath
        removeFromTree workingTree filePath
        broadcastEvent event

      _ -> return ()

watchDirectory
  :: T.Text -> [RawFilePath] -> Tree -> RawFilePath -> App WatchDescriptor
watchDirectory checkoutName projectPath workingTree filePath = do
  info $ "WATCH " <> T.decodeUtf8 filePath
  Context { cntxINotify } <- ask
  withRunInIO $ \runInIO -> addWatch
    cntxINotify
    watchTypes
    filePath
    (\event -> runInIO
      $ handleEvent checkoutName projectPath workingTree filePath event
    )
  where watchTypes = [Modify, Attrib, Move, MoveOut, Delete, Create]

dumpToDirectory :: Tree -> RawFilePath -> App ()
dumpToDirectory tree filepath = forTree tree $ \(filename, treeContent) ->
  case treeContent of
    ContentTree subtree -> do
      let subpath = filepath </> filename
      liftIO $ createDirectory subpath
      dumpToDirectory subtree subpath
    ContentFile fileHash -> do
      contents <- retrive fileHash
      info $ "OUT: " <> T.decodeUtf8 filename
      liftIO $ RFP.withFile (filepath </> filename) WriteMode $ \hd ->
        BS.hPut hd contents


writeOutTree :: RawFilePath -> Tree -> App ()
writeOutTree dirPath tree = withRunInIO $ \runInIO -> do
  info $ "whiteOutTree: dirPath=" <> T.decodeUtf8 dirPath
  forTree tree $ \(filename, contents) -> runInIO $ do
    let fullFilePath = dirPath </> filename
    case contents of
      ContentFile fileHash -> do
        fileContents <- retrive fileHash
        info
          $  "OUTPUT: "
          <> T.decodeUtf8 fullFilePath
          <> " "
          <> renderFileHash fileHash
        liftIO $ RFP.writeFile fullFilePath fileContents
      ContentTree subTree -> do
        liftIO $ createDirectoryIfMissing True fullFilePath
        writeOutTree fullFilePath subTree

onNewFile :: T.Text -> [RawFilePath] -> RawFilePath -> ObjectHash -> App ()
onNewFile checkoutName projectPath filename fileHash = do
  checkoutDir <- getCheckoutDir
  checkouts   <- liftIO $ listDirectory checkoutDir
  forM_ checkouts $ \checkout -> if T.decodeUtf8 checkout == checkoutName
    then return ()
    else do
      let dirname = checkoutDir </> checkout </> foldl (</>) "" projectPath
      file <- retrive fileHash
      let filepath = dirname </> filename
      info
        $  "REPLICATE: "
        <> T.decodeUtf8 filepath
        <> " "
        <> renderFileHash fileHash
      liftIO $ RFP.writeFile filepath file


testOutputLoop :: RawFilePath -> App ()
testOutputLoop filepath = catch inner $ \e -> liftIO $ do
  putStrLn "ERROR:"
  print (e :: SomeException)
 where
  inner = do
    liftIO $ threadDelay $ 1000 * 1000 * 10
    Context { cntxRootTree } <- ask
    liftIO $ createDirectoryIfMissing True filepath
    info $ "The tree: " <> showT cntxRootTree
    writeOutTree filepath cntxRootTree
    testOutputLoop filepath

startDirSync :: T.Text -> Tree -> RawFilePath -> [RawFilePath] -> App ()
startDirSync checkoutName workingTree thisDir projectPath = do
  info $ "SCAN: " <> T.decodeUtf8 thisDir

  diskFiles  <- liftIO $ Sys.listDirectory $ BS.unpack thisDir
  _          <- watchDirectory checkoutName projectPath workingTree thisDir

  storeFiles <- liftIO $ H.toList $ unTree workingTree

  forM_ (map BS.pack diskFiles) $ \filename -> do
    let filepath = thisDir </> filename
    isDirectory <- liftIO $ doesDirectoryExist filepath
    if not isDirectory
      then do
        fileHash <- storeFile workingTree filepath
        onNewFile checkoutName projectPath filename fileHash
      else do
        newTree <- liftIO $ fmap Tree H.new
        liftIO $ H.insert (unTree workingTree) filename (ContentTree newTree)
        startDirSync checkoutName newTree filepath $ filename : projectPath

  forM_ storeFiles $ \(filename, contents) -> do
    let filepath = thisDir </> filename
    case contents of
      ContentFile filehash -> do
        fileBytes <- retrive filehash
        liftIO $ RFP.writeFile filepath fileBytes
      _ -> return ()


startProjectSync :: T.Text -> RawFilePath -> App ()
startProjectSync checkoutName thisDir = do
  rootTree <- getRootTree
  startDirSync checkoutName rootTree thisDir []

runAppStartup :: App ()
runAppStartup = do
  checkoutDir <- getCheckoutDir
  checkouts   <- liftIO $ listDirectory checkoutDir
  forM_ checkouts $ \filename -> do
    let checkoutName = T.decodeUtf8 filename
    startProjectSync checkoutName $ checkoutDir </> filename

checkoutProject :: T.Text -> RawFilePath -> App ()
checkoutProject checkoutName checkoutTo = do
  checkoutDir <- getCheckoutDir
  info $ "checkoutDir = " <> T.decodeUtf8 checkoutDir
  let linkFilepath = checkoutDir </> T.encodeUtf8 checkoutName
  info
    $  "CHECKOUT:"
    <> checkoutName
    <> ": "
    <> T.decodeUtf8 linkFilepath
    <> " -> "
    <> T.decodeUtf8 checkoutTo
  liftIO $ whenM (doesFileExist linkFilepath) $ error
    "project already checked out under that name"
  liftIO $ Posix.createSymbolicLink checkoutTo linkFilepath
  liftIO $ unlessM (doesFileExist checkoutTo) $
    Posix.createDirectory checkoutTo Posix.ownerModes
  startProjectSync checkoutName linkFilepath


sendToUser :: BS.ByteString -> UserRequest ()
sendToUser message = do
  UserReqContext { ucntxHandle } <- ask
  liftIO $ BS.hPutStr ucntxHandle message

printTree :: Int -> Tree -> UserRequest ()
printTree indent tree =
  withRunInIO
    $ \runInIO -> flip H.mapM_ (unTree tree) $ \(filename, contents) ->
        runInIO $ do
          replicateM_ indent $ sendToUser " "
          sendToUser $ filename <> " -> "
          case contents of
            ContentFile objectHash ->
              sendToUser $ renderObjectHashBS objectHash <> "\n"
            ContentTree innerTree -> do
              sendToUser "\n"
              printTree (indent + 2) innerTree

processMessage :: ClientMsg -> UserRequest ()
processMessage ClientMsg { cmsgCmd, cmsgCwd = Base64JSON cwd } =
  case cmsgCmd of
    TreeCmd -> do
      Context { cntxRootTree } <- lift ask
      printTree 0 cntxRootTree
    DumpCmd -> do
      Context { cntxRootTree } <- lift ask
      lift $ dumpToDirectory cntxRootTree cwd
    CheckoutCmd checkout -> processCheckoutMessage cwd checkout
      

processCheckoutMessage :: RawFilePath -> Checkout -> UserRequest ()
processCheckoutMessage cwd (Checkout checkoutName (Base64JSON checkoutDir)) = do
  let absCheckoutDir = cwd </> checkoutDir
  lift $ checkoutProject checkoutName absCheckoutDir 

clientDecoder :: P.Parser BS.ByteString UserRequest ()
clientDecoder = P.decodeGet Bin.getWord16be >>= \case
  Left  _             -> error "decode error"
  Right messageLength -> do
    liftIO $ putStrLn "got message length from the socket"
    P.decodeGet (Bin.getByteString (fromIntegral messageLength)) >>= \case
      Left  _         -> error "decode error"
      Right messageBS -> do
        liftIO $ putStrLn "got message from the socket"
        case eitherDecodeStrict messageBS of
          Left errorMsg -> do
            P.lift $ sendToUser $ "was not JSON" <> messageBS
            P.lift $ sendToUser $ BS.pack $ "Error was " <> errorMsg
          Right msg -> do
            liftIO $ putStrLn "decoded correctly"
            P.lift $ processMessage msg
            clientDecoder



clientSocketThread :: Socket -> App ()
clientSocketThread sock = do
  liftIO $ putStrLn "in read thread"
  hd <- liftIO $ socketToHandle sock ReadWriteMode
  let fromClient = P.fromHandle hd
  flip runReaderT (UserReqContext {ucntxHandle = hd}) $ do
    -- P.runEffect $ P.for fromClient (\) >-> toClient
    sendToUser "Hello, you have connected to the socket\n"
    _ <- P.runStateT clientDecoder fromClient
    liftIO $ putStrLn "Read thread exited"

acceptLoop :: Socket -> App ()
acceptLoop sock = do
  liftIO $ bind sock $ SockAddrUnix "/tmp/mysock"
  liftIO $ listen sock 5
  forever $ do
    liftIO $ putStrLn "waiting to accept"
    (sock', _) <- liftIO $ accept sock
    liftIO $ putStrLn $ "got connection"
    async $ clientSocketThread sock'

ensureStateDirIsSetup :: App ()
ensureStateDirIsSetup = do
  checkoutDir <- getCheckoutDir
  liftIO $ whenM (not <$> Posix.fileExist checkoutDir) $ Posix.createDirectory
    checkoutDir
    Posix.ownerModes

runApp :: App ()
runApp = do
  liftIO $ tryRemoveFile "/tmp/mysock"
  ensureStateDirIsSetup
  runAppStartup
  bracket (liftIO $ socket AF_UNIX Stream defaultProtocol)
          (liftIO . close)
          acceptLoop

main :: IO ()
main = do
  objectStore <- H.new
  rootTree    <- fmap Tree H.new
  getHomeDirectory >>= \case
    Nothing      -> error "could not get $HOME"
    Just homeDir -> withINotify $ \inotify -> runReaderT
      runApp
      (Context
        { cntxINotify      = inotify
        , cntxObjectStore  = objectStore
        , cntxRootTree     = rootTree
        , cntxStateRoot    = homeDir <> "/var/wh"
        , cntxINotifyQueue = Nothing
        }
      )


