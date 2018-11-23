{-# LANGUAGE ConstraintKinds #-}

module ObjectStore
  ( storeFile
  , storeDir
  , retrive
  , ObjectStore(..)
  , TreeContent(..)
  , newInMemoryStore
  , HasStore
  , getStore
  )
where

import qualified Data.ByteString.RawFilePath   as RFP
                                         hiding ( putStrLn )
import qualified Data.ByteString.Char8         as BS
import           RawFilePath.Directory
import qualified Data.Text.Encoding            as T
import qualified Data.HashTable.IO             as H
import           Control.Monad.IO.Unlift
import           LibWormhole
import           Logger
import Tree
import Object

type HashTable k v = H.BasicHashTable k v

type StoreConstr m = (MonadIO m, HasStore m, HasLogger m)

newtype ObjectStore = ObjectStore {
  unObjectStore :: HashTable ObjectHash BS.ByteString
}

class HasStore m where
  getStore :: m ObjectStore


newInMemoryStore :: MonadIO m => m ObjectStore
newInMemoryStore = liftIO . fmap ObjectStore $ H.new


store :: (MonadIO m, HasStore m) => ObjectHash -> BS.ByteString -> m ()
store filehash contents = do
  s <- getStore
  liftIO $ H.insert (unObjectStore s) filehash contents


storeFile :: StoreConstr m => Tree -> RawFilePath -> m ObjectHash
storeFile workingTree filepath = do
  fileBytes <- liftIO $ RFP.readFile filepath
  let fileHash = hashBytes fileBytes
  info $ "STORE: " <> renderObjectHash fileHash <> " " <> T.decodeUtf8 filepath
  store fileHash fileBytes
  let (_, filename) = splitFilepath filepath
  addToTree workingTree filename fileHash
  return fileHash

retrive :: StoreConstr m => ObjectHash -> m BS.ByteString
retrive filehash = do
  store <- getStore
  liftIO $ H.lookup (unObjectStore store) filehash >>= \case
    Nothing       -> error "ERROR: Could not find hash"
    Just contents -> return contents

storeDir :: StoreConstr m => Tree -> Tree -> RawFilePath -> m ()
storeDir workingTree subtree filepath = do
  --info $ "STOREDIR: " <> T.decodeUtf8 filepath
  let (_, filename) = splitFilepath filepath
  addDirToTree workingTree filename subtree
