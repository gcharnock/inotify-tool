{-# LANGUAGE ConstraintKinds #-}

module ObjectStore
  ( storeFile
  , storeDir
  , retrive
  , ObjectStore(..)
  , newInMemoryStore
  , HasStore
  , getStore
  , treeToDisk
  )
where

import qualified Data.ByteString.RawFilePath   as RFP
                                         hiding ( putStrLn )
import qualified Data.ByteString.Char8         as BS
import           RawFilePath.Directory
import qualified Data.HashTable.IO             as H
import           Control.Monad.IO.Unlift
import           LibWormhole
import qualified Data.Tree as Tree
import           Data.Object
import Filesystem
import Control.Monad.Reader.Class
import Logging.Contextual
import Logging.Contextual.BasicScheme

type HashTable k v = H.BasicHashTable k v

type StoreConstr env m = (MonadIO m, MonadReader env m, HasStore env, HasLog env)

newtype ObjectStore = ObjectStore {
  unObjectStore :: HashTable ObjectHash BS.ByteString
}

class HasStore env where
  getStore :: env -> ObjectStore

askStore :: (MonadReader env m, HasStore env) => m ObjectStore
askStore = getStore <$> ask

newInMemoryStore :: MonadIO m => m ObjectStore
newInMemoryStore = liftIO . fmap ObjectStore $ H.new


store :: (MonadReader env m, MonadIO m, HasStore env) => ObjectHash -> BS.ByteString -> m ()
store filehash contents = do
  s <- fmap getStore ask
  liftIO $ H.insert (unObjectStore s) filehash contents


storeFile :: StoreConstr env m => Tree.Tree -> RawFilePath -> m ObjectHash
storeFile workingTree filepath = do
  fileBytes <- liftIO $ RFP.readFile filepath
  let fileHash = hashBytes fileBytes
  [logInfo|STORE: {renderObjectHash fileHash}T.decodeUtf8 filepath|]
  store fileHash fileBytes
  let (_, filename) = splitFilepath filepath
  Tree.add workingTree filename fileHash
  return fileHash

retrive :: StoreConstr env m => ObjectHash -> m BS.ByteString
retrive filehash = do
  ObjectStore { unObjectStore = theStore } <- askStore 
  liftIO $ (H.lookup theStore filehash) >>= \case
    Nothing       -> error "ERROR: Could not find hash"
    Just contents -> return contents

storeDir :: StoreConstr env m => Tree.Tree -> Tree.Tree -> RawFilePath -> m ()
storeDir workingTree subtree filepath = do
  --info $ "STOREDIR: " <> T.decodeUtf8 filepath
  let (_, filename) = splitFilepath filepath
  Tree.addSubTree workingTree filename subtree


treeToDisk :: (MonadUnliftIO m, StoreConstr env m) => RawFilePath -> Tree.Tree -> m ()
treeToDisk dirPath tree = do
  [logInfo|whiteOutTree: dirPath={dirPath}|]
  Tree.forM_ tree $ \(filename, contents) -> do
    let fullFilePath = dirPath </> filename
    case contents of
      Tree.ContentFile fileHash -> do
        fileContents <- retrive fileHash
        [logInfo|OUTPUT {fullFilePath} {renderObjectHash fileHash}|]
        liftIO $ RFP.writeFile fullFilePath fileContents
      Tree.ContentTree subTree -> do
        liftIO $ createDirectoryIfMissing True fullFilePath
        treeToDisk fullFilePath subTree