module Main where

import Control.Monad.Trans (lift)
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
import qualified Pipes as P
import qualified Pipes.Binary as P
import qualified Pipes.ByteString as P
import qualified Pipes.Parse as P
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin

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
  cntxRootTree :: Tree
}

type App = ReaderT Context IO

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



retrive :: FileHash -> App BS.ByteString
retrive hash = do
  Context { cntxObjectStore } <- ask
  liftIO $ H.lookup cntxObjectStore hash >>= \case
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
            performInitialDirectorySweep newWorkingTree filepath 
          else storeFile workingTree filepath

      Modified { isDirectory, maybeFilePath } -> case maybeFilePath of
        Nothing -> info $ "UNHANDLED: " <> showT maybeFilePath <> "was nothing"
        Just filename -> storeFile workingTree (dirPath </> filename)

      Deleted { isDirectory, filePath } -> do
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

performInitialDirectorySweep :: Tree -> RawFilePath -> App ()
performInitialDirectorySweep workingTree thisDir = do
  Context { cntxINotify = inotify } <- ask

  liftIO $ BS.putStrLn $ "SCAN " <> thisDir <> ""

  files <- liftIO $ Sys.listDirectory $ BS.unpack thisDir
  --files <- liftIO $ listDirectory thisDir
  -- liftIO $ print files

  watchDirectory workingTree thisDir

  forM_ (map BS.pack files) $ \filename -> do
    let filepath = thisDir </> filename
    isDirectory <- liftIO $ doesDirectoryExist filepath
    if not isDirectory
      then storeFile workingTree filepath
      else do
        newTree <- liftIO $ fmap Tree H.new
        liftIO $ H.insert (unTree workingTree) filename (ContentTree newTree)
        performInitialDirectorySweep newTree filepath

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
  performInitialDirectorySweep cntxRootTree "testdir/"


sendToUser :: BS.ByteString -> UserRequest ()
sendToUser message = do
  UserReqContext { ucntxHandle } <- ask
  liftIO $ BS.hPutStr ucntxHandle message

printTree :: Int -> Tree -> UserRequest ()
printTree indent tree =
  withRunInIO $ \runInIO -> 
    flip H.mapM_ (unTree tree) $ \(filename, contents) ->
      runInIO $ do
        replicateM_ indent $ sendToUser " "
        sendToUser $ filename <> " -> "
        case contents of
          ContentFile (FileHash fileHash) -> sendToUser $ showBS fileHash <> "\n"
          ContentTree tree -> do
            sendToUser "\n"
            printTree (indent + 2) tree

processMessage :: Cmd -> UserRequest ()
processMessage TreeCmd = do
  Context { cntxObjectStore, cntxRootTree } <- lift ask
  printTree 0 cntxRootTree

clientDecoder :: P.Parser BS.ByteString UserRequest ()
clientDecoder =
  P.decodeGet Bin.getWord16be >>= \case
    Left decodeError -> error "decode error"
    Right messageLength -> do
      liftIO $ putStrLn "got message length from the socket"
      P.decodeGet (Bin.getByteString (fromIntegral messageLength)) >>= \case
        Left decodeError -> error "decode error"
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
  handle <- liftIO $ socketToHandle sock ReadWriteMode
  let fromClient = P.fromHandle handle
  flip runReaderT (UserReqContext {ucntxHandle = handle}) $ do
    -- P.runEffect $ P.for fromClient (\) >-> toClient
    sendToUser "Hello, you have connected to the socket\n"
    P.runStateT clientDecoder fromClient
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
  withINotify $ \inotify -> runReaderT
    runApp
    (Context
      { cntxINotify     = inotify
      , cntxObjectStore = objectStore
      , cntxRootTree    = rootTree
      }
    )

