module Main where

import           Data.Monoid
import           UnliftIO.Exception
import           UnliftIO.Async
import           Control.Monad.IO.Class
import qualified System.FilePath               as FP
import qualified Data.ByteString.UTF8          as UTF8
import qualified Data.ByteString.RawFilePath   as RFP
import           RawFilePath.Directory
import           RawFilePath
import qualified Data.ByteString               as BS
                                         hiding ( putStrLn )
import qualified Data.ByteString.Char8         as BS
import           Data.String.Interpolate.IsString
import           Control.Monad
import           System.INotify
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

type App a = ReaderT Context IO a

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
    event@Created { isDirectory, filePath } -> do
      if isDirectory
        then do
          watchDirectory workingTree filePath
          performInitialDirectorySweep workingTree filePath
        else return () -- not sure we need to do anything here (?)
    event@Modified { isDirectory, maybeFilePath } -> do
      case maybeFilePath of
        Nothing -> info $ "UNHANDLED: " <> showT maybeFilePath <> "was nothing"
        Just filename -> do
          fileBytes <- liftIO $ RFP.readFile filename
          let fileHash = FileHash $ hashWith SHA256 fileBytes
          info
            $  "STORE: "
            <> renderFileHash fileHash
            <> " "
            <> T.decodeUtf8 filename
          store fileHash fileBytes
          liftIO $ H.insert (unTree workingTree) filename (ContentFile fileHash)
    event -> return ()

watchRegularFile :: Tree -> RawFilePath -> App WatchDescriptor
watchRegularFile workingTree filePath = do
  info $ "watching regular file " <> T.decodeUtf8 filePath
  Context { cntxINotify } <- ask
  withRunInIO $ \runInIO -> addWatch
    cntxINotify
    watchTypes
    filePath
    (\event -> (runInIO $ handleEvent workingTree event))
  where watchTypes = [Modify, Attrib, Move, MoveOut, Delete]

watchDirectory :: Tree -> RawFilePath -> App WatchDescriptor
watchDirectory workingTree filePath = do
  info $ "watching directory " <> T.decodeUtf8 filePath
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

  files <- liftIO $ listDirectory thisDir

  forM_ files $ \filename -> do
    let filepath = thisDir </> filename
    isDirectory <- liftIO $ doesDirectoryExist filepath
    if isDirectory
      then do
        tree <- liftIO $ fmap Tree H.new
        watchDirectory workingTree filepath

        liftIO $ H.insert (unTree workingTree) filename (ContentTree tree)

        performInitialDirectorySweep tree filepath
      else do
        fileBytes <- liftIO $ RFP.readFile filepath
        let fileHash = FileHash $ hashWith SHA256 fileBytes
        info
          $  "STORE: "
          <> renderFileHash fileHash
          <> " "
          <> T.decodeUtf8 filename
          <> " from "
          <> T.decodeUtf8 thisDir
        store fileHash fileBytes

        liftIO $ H.insert (unTree workingTree) filename (ContentFile fileHash)

        --void $ watchRegularFile filepath

writeOutTree :: RawFilePath -> Tree -> App ()
writeOutTree dirPath tree = withRunInIO $ \runInIO -> do
  info $ "whiteOutTree: dirPath=" <> T.decodeUtf8 dirPath
  flip H.mapM_ (unTree tree) $ \(filename, contents) -> runInIO $ do
    let fullFilePath = dirPath </> filename
    case contents of
      ContentFile fileHash -> do
        contents <- retrive fileHash
        info $ "OUTPUT: " <> T.decodeUtf8 fullFilePath <> " " <> renderFileHash fileHash
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

runApp :: App ()
runApp = do
  async $ testOutputLoop "/tmp/inotify-tool-test"

  Context { cntxRootTree } <- ask

  performInitialDirectorySweep cntxRootTree "."
  liftIO $ threadDelay $ 1000 * 1000 * 1000 * 1000
  return ()

main :: IO ()
main = do
  objectStore <- H.new
  rootTree    <- fmap Tree H.new
  withINotify $ \inotify -> do
    runReaderT
      runApp
      (Context
        { cntxINotify     = inotify
        , cntxObjectStore = objectStore
        , cntxRootTree    = rootTree
        }
      )
