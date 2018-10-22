module Main where

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
import           RawFilePath.Directory
import           RawFilePath             hiding ( putStrLn
                                                , unpack
                                                , pack
                                                )
import           Data.String.Interpolate.IsString
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
import           LibWormhole                    ( Cmd(TreeCmd) )

type HashTable k v = H.BasicHashTable k v
newtype FileHash = FileHash { unFileHash :: (Digest SHA256) }
  deriving (Show, Eq)

showT :: Show a => a -> T.Text
showT = T.pack . show

instance Hashable FileHash where
  hashWithSalt salt (FileHash a) = hashWithSalt salt $ (convert a :: BS.ByteString)

data TreeContent = ContentFile FileHash | ContentTree Tree

newtype Tree = Tree { unTree :: HashTable RawFilePath TreeContent } deriving (Show)

data Context = Context {
  cntxINotify :: INotify,
  cntxObjectStore :: HashTable FileHash BS.ByteString,
  cntxRootTree :: Tree
}

type App = ReaderT Context IO

data UserReqContext = UserReqContext {
  ucntxHandle :: Handle
}

type UserRequest a = ReaderT UserReqContext App a

infixr 5 </>

(</>) :: RawFilePath -> RawFilePath -> RawFilePath
a </> b = UTF8.fromString $ (UTF8.toString a) FP.</> (UTF8.toString b)

renderFileHash :: FileHash -> T.Text
renderFileHash = T.decodeUtf8 . convertToBase Base64 . unFileHash

sha256File :: MonadIO m => RawFilePath -> m (Digest SHA256)
sha256File filepath = do
  fileBytes <- liftIO $ RFP.readFile filepath
  return $ hashWith SHA256 fileBytes

info :: MonadIO m => T.Text -> m ()
info = liftIO . T.putStrLn

store :: FileHash -> BS.ByteString -> App ()
store hash contents = do
  Context { cntxObjectStore } <- ask
  liftIO $ H.insert cntxObjectStore hash contents


storeFile :: Tree -> RawFilePath -> App ()
storeFile workingTree filename = do
  fileBytes <- liftIO $ RFP.readFile filename
  let fileHash = FileHash $ hashWith SHA256 fileBytes
  info $ "STORE: " <> renderFileHash fileHash <> " " <> T.decodeUtf8 filename
  store fileHash fileBytes
  liftIO $ H.insert (unTree workingTree) filename (ContentFile fileHash)

storeDirectory :: Tree -> RawFilePath -> RawFilePath -> App ()
storeDirectory workingTree dirname filename = do
  watchDirectory workingTree (dirname </> filename)

  tree <- liftIO $ fmap Tree H.new
  liftIO $ H.insert (unTree workingTree) filename (ContentTree tree)

  performInitialDirectorySweep tree (dirname </> filename)

retrive :: FileHash -> App BS.ByteString
retrive hash = do
  Context { cntxObjectStore } <- ask
  liftIO $ H.lookup cntxObjectStore hash >>= \case
    Nothing       -> error "ERROR: Could not find hash"
    Just contents -> return contents

handleEvent :: Tree -> Event -> App ()
handleEvent workingTree event = do
  liftIO $ print event
  case event of
    event@Created { isDirectory, filePath } -> if isDirectory
      then do
        watchDirectory workingTree filePath
        performInitialDirectorySweep workingTree filePath
      else storeFile workingTree filePath

    event@Modified { isDirectory, maybeFilePath } -> case maybeFilePath of
      Nothing -> info $ "UNHANDLED: " <> showT maybeFilePath <> "was nothing"
      Just filename -> storeFile workingTree filename

    event@Deleted { isDirectory, filePath } -> if isDirectory
      then error "UNHANDLED: not sure what to do"
      else do
        info $ "REMOVE: " <> T.decodeUtf8 filePath
        liftIO $ H.delete (unTree workingTree) filePath

    event -> return ()

watchDirectory :: Tree -> RawFilePath -> App WatchDescriptor
watchDirectory workingTree filePath = do
  info $ "WATCH " <> T.decodeUtf8 filePath
  Context { cntxINotify } <- ask
  withRunInIO $ \runInIO -> addWatch
    cntxINotify
    watchTypes
    filePath
    (\event -> runInIO $ handleEvent workingTree event)
  where watchTypes = [Modify, Attrib, Move, MoveOut, Delete, Create, Delete]

performInitialDirectorySweep :: Tree -> RawFilePath -> App ()
performInitialDirectorySweep workingTree thisDir = do
  Context { cntxINotify = inotify } <- ask

  liftIO $ BS.putStrLn $ "SCAN " <> thisDir <> ""

  files <- liftIO $ Sys.listDirectory $ BS.unpack thisDir
  --files <- liftIO $ listDirectory thisDir
  -- liftIO $ print files

  forM_ (map BS.pack files) $ \filename -> do
    let filepath = thisDir </> filename
    isDirectory <- liftIO $ doesDirectoryExist filepath
    if isDirectory
      then storeDirectory workingTree thisDir filename
      else storeFile workingTree filepath

writeOutTree :: RawFilePath -> Tree -> App ()
writeOutTree dirPath tree = withRunInIO $ \runInIO -> do
  info $ "whiteOutTree: dirPath=" <> T.decodeUtf8 dirPath
  flip H.mapM_ (unTree tree) $ \(filename, contents) -> runInIO $ do
    let fullFilePath = dirPath </> filename
    case contents of
      ContentFile fileHash -> do
        contents <- retrive fileHash
        info
          $  "OUTPUT: "
          <> T.decodeUtf8 fullFilePath
          <> " "
          <> renderFileHash fileHash
        liftIO $ RFP.writeFile fullFilePath contents
      ContentTree subTree -> do
        liftIO $ createDirectoryIfMissing True fullFilePath
        writeOutTree fullFilePath subTree



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

runAppStartup :: App ()
runAppStartup = do
  Context { cntxRootTree } <- ask
  performInitialDirectorySweep cntxRootTree "/tmp/inotify-tool-test"


sendToUser :: BS.ByteString -> UserRequest ()
sendToUser message = do
  UserReqContext { ucntxHandle } <- ask
  liftIO $ BS.hPutStrLn ucntxHandle message

processMessage :: Cmd -> UserRequest ()
processMessage TreeCmd = do
  sendToUser "Printing out directory tree"

clientSocketThread :: Socket -> App ()
clientSocketThread sock = do
  liftIO $ putStrLn "in read thread"
  handle <- liftIO $ socketToHandle sock ReadWriteMode
  liftIO $ BS.hPutStrLn handle "Hello, you have connected to the socket"
  _       <- liftIO $ hWaitForInput handle (-1)
  recived <- liftIO $ BS.hGetContents handle
  let eitherMSG = eitherDecodeStrict recived
  case eitherMSG of
    Left errorMsg -> do
      liftIO $ BS.putStrLn $ "was not JSON" <> recived
      liftIO $ putStrLn $ "Error was " <> errorMsg
    Right msg ->
      runReaderT (processMessage msg) UserReqContext {ucntxHandle = handle}

acceptLoop :: Socket -> App ()
acceptLoop sock = do
  liftIO $ bind sock $ SockAddrUnix "/tmp/mysock"
  liftIO $ listen sock 5
  forever $ do
    liftIO $ putStrLn "waiting to accept"
    (sock', _) <- liftIO $ accept sock
    liftIO $ putStrLn $ "got connection"
    async $ clientSocketThread sock'

runApp :: App ()
runApp = do
  liftIO $ tryRemoveFile "/tmp/mysock"
  runAppStartup
  bracket (liftIO $ socket AF_UNIX SeqPacket defaultProtocol)
          (liftIO . close)
          acceptLoop

main :: IO ()
main = do
  objectStore <- H.new
  rootTree    <- fmap Tree H.new
  withINotify $ \inotify -> runReaderT
    runApp
    (Context
      { cntxINotify     = inotify
      , cntxObjectStore = objectStore
      , cntxRootTree    = rootTree
      }
    )

