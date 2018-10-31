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
import           RawFilePath.Directory
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
import qualified System.Posix.ByteString as Posix

type HashTable k v = H.BasicHashTable k v
newtype FileHash = FileHash { unFileHash :: (Digest SHA256) }
  deriving (Show, Eq)

showT :: Show a => a -> T.Text
showT = T.pack . show

showBS :: Show a => a -> BS.ByteString
showBS = BS.pack . show

instance Hashable FileHash where
  hashWithSalt salt (FileHash a) = hashWithSalt salt $ (convert a :: BS.ByteString)

data TreeContent = ContentFile FileHash | ContentTree Tree
  deriving Show

newtype Tree = Tree { unTree :: HashTable RawFilePath TreeContent } deriving (Show)

data Context = Context {
  cntxINotify :: INotify,
  cntxObjectStore :: HashTable FileHash BS.ByteString,
  cntxRootTree :: Tree,
  cntxStateRoot :: RawFilePath
}

whenM :: Monad m => m Bool -> m () -> m ()
whenM test action = test >>= \case
  True  -> action
  False -> return ()

type App = ReaderT Context IO

getRootTree :: App Tree
getRootTree = fmap cntxRootTree ask

getStateRoot :: App RawFilePath
getStateRoot = fmap cntxStateRoot ask

getCheckoutDir :: App RawFilePath
getCheckoutDir = do
  stateRoot <- getStateRoot
  return $ stateRoot </> "checkouts"

data UserReqContext = UserReqContext {
  ucntxHandle :: Handle
}

type UserRequest = ReaderT UserReqContext App

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

forTree
  :: MonadUnliftIO m => Tree -> ((RawFilePath, TreeContent) -> m ()) -> m ()
forTree tree action =
  withRunInIO $ \runInIO -> H.mapM_ (\a -> runInIO $ action a) (unTree tree)

store :: FileHash -> BS.ByteString -> App ()
store filehash contents = do
  Context { cntxObjectStore } <- ask
  liftIO $ H.insert cntxObjectStore filehash contents


storeFile :: Tree -> RawFilePath -> App ()
storeFile workingTree filepath = do
  fileBytes <- liftIO $ RFP.readFile filepath
  let fileHash = FileHash $ hashWith SHA256 fileBytes
  info $ "STORE: " <> renderFileHash fileHash <> " " <> T.decodeUtf8 filepath
  store fileHash fileBytes
  let (_, filename) = splitFilepath filepath
  liftIO $ H.insert (unTree workingTree) filename (ContentFile fileHash)

storeDir :: Tree -> Tree -> RawFilePath -> App ()
storeDir workingTree subtree filepath = do
  info $ "STOREDIR: " <> T.decodeUtf8 filepath
  let (_, filename) = splitFilepath filepath
  liftIO $ H.insert (unTree workingTree) filename (ContentTree subtree)

retrive :: FileHash -> App BS.ByteString
retrive filehash = do
  Context { cntxObjectStore } <- ask
  liftIO $ H.lookup cntxObjectStore filehash >>= \case
    Nothing       -> error "ERROR: Could not find hash"
    Just contents -> return contents

handleEvent :: Tree -> RawFilePath -> Event -> App ()
handleEvent workingTree dirPath event = try handler >>= \case
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
            newWorkingTree <- liftIO $ Tree <$> H.new
            storeDir workingTree newWorkingTree filepath
            startDirSync newWorkingTree filepath
          else storeFile workingTree filepath

      Modified { maybeFilePath } -> case maybeFilePath of
        Nothing -> info $ "UNHANDLED: " <> showT maybeFilePath <> "was nothing"
        Just filename -> storeFile workingTree (dirPath </> filename)

      Deleted { filePath } -> do
        info $ "REMOVE: " <> T.decodeUtf8 filePath
        liftIO $ H.delete (unTree workingTree) filePath

      _ -> return ()

watchDirectory :: Tree -> RawFilePath -> App WatchDescriptor
watchDirectory workingTree filePath = do
  info $ "WATCH " <> T.decodeUtf8 filePath
  Context { cntxINotify } <- ask
  withRunInIO $ \runInIO -> addWatch
    cntxINotify
    watchTypes
    filePath
    (\event -> runInIO $ handleEvent workingTree filePath event)
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

startDirSync :: Tree -> RawFilePath -> App ()
startDirSync workingTree thisDir = do
  info $ "SCAN: " <> T.decodeUtf8 thisDir

  files <- liftIO $ Sys.listDirectory $ BS.unpack thisDir
  _     <- watchDirectory workingTree thisDir

  forM_ (map BS.pack files) $ \filename -> do
    let filepath = thisDir </> filename
    isDirectory <- liftIO $ doesDirectoryExist filepath
    if not isDirectory
      then storeFile workingTree filepath
      else do
        newTree <- liftIO $ fmap Tree H.new
        liftIO $ H.insert (unTree workingTree) filename (ContentTree newTree)
        startDirSync newTree filepath


startProjectSync :: RawFilePath -> App ()
startProjectSync thisDir = do
  rootTree <- getRootTree
  startDirSync rootTree thisDir

runAppStartup :: App ()
runAppStartup = do
  checkoutDir <- getCheckoutDir
  checkouts   <- liftIO $ listDirectory checkoutDir
  forM_ checkouts $ \filename -> startProjectSync $ checkoutDir </> filename 

checkoutProject :: T.Text -> RawFilePath -> App ()
checkoutProject checkoutName checkoutTo = do
  checkoutDir <- getCheckoutDir
  let linkFilepath = checkoutDir </> T.encodeUtf8 checkoutName 
  liftIO $ whenM (doesFileExist linkFilepath) $ error "project already checked out under that name"
  liftIO $ Posix.createSymbolicLink checkoutTo linkFilepath
  startProjectSync $ linkFilepath
  

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
            ContentFile (FileHash fileHash) ->
              sendToUser $ showBS fileHash <> "\n"
            ContentTree innerTree -> do
              sendToUser "\n"
              printTree (indent + 2) innerTree

processMessage :: ClientMsg -> UserRequest ()
processMessage ClientMsg { cmsgCmd, cmsgCwd } = case cmsgCmd of
  TreeCmd -> do
    Context { cntxRootTree } <- lift ask
    printTree 0 cntxRootTree
  DumpCmd -> do
    Context { cntxRootTree } <- lift ask
    lift $ dumpToDirectory cntxRootTree (T.encodeUtf8 cmsgCwd)

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

runApp :: App ()
runApp = do
  liftIO $ tryRemoveFile "/tmp/mysock"
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
        { cntxINotify     = inotify
        , cntxObjectStore = objectStore
        , cntxRootTree    = rootTree
        , cntxStateRoot   = homeDir <> "/var/wh"
        }
      )


