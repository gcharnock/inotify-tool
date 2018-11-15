module ObjectStore
  ( storeFile
  , storeDir
  , ObjectStore(..)
  , Tree (..)
  , TreeContent(..)
  , newInMemoryStore
  , FileHash
  )
where

import qualified Data.Text as T
import qualified Data.ByteString.RawFilePath   as RFP
                                         hiding ( putStrLn )
import qualified Data.ByteString.Char8         as BS
import           RawFilePath.Directory
import qualified Data.Text.Encoding            as T
import           Crypto.Hash                    ( Digest
                                                , SHA256(..)
                                                , hashWith
                                                )
import           Data.ByteArray.Encoding        ( convertToBase
                                                , Base(..)
                                                )
import qualified Data.HashTable.IO             as H
import           Control.Monad.IO.Unlift
import           Data.Hashable
import           Data.ByteArray
import           LibWormhole
import Logger

type HashTable k v = H.BasicHashTable k v

newtype FileHash = FileHash { unFileHash :: (Digest SHA256) }
  deriving (Show, Eq)

instance Hashable FileHash where
  hashWithSalt salt (FileHash a) = hashWithSalt salt $ (convert a :: BS.ByteString)

instance Loggable FileHash where
  toText = renderFileHash

hashFile :: MonadIO m => RawFilePath -> m FileHash
hashFile filepath = do
  fileBytes <- liftIO $ RFP.readFile filepath
  return $ FileHash $ hashWith SHA256 fileBytes

hashBytes :: BS.ByteString -> FileHash
hashBytes bs = FileHash $ hashWith SHA256 bs

newtype ObjectStore = ObjectStore {
  unObjectStore :: HashTable FileHash BS.ByteString
} 

class HasStore m where
  getStore :: m ObjectStore 

data TreeContent = ContentFile FileHash | ContentTree Tree
  deriving Show

newtype Tree = Tree { unTree :: HashTable RawFilePath TreeContent } deriving (Show)

renderFileHash :: FileHash -> T.Text
renderFileHash = T.decodeUtf8 . convertToBase Base64 . unFileHash

newInMemoryStore :: MonadIO m => m ObjectStore
newInMemoryStore = liftIO . fmap ObjectStore $ H.new


store :: (MonadIO m, HasStore m) => FileHash -> BS.ByteString -> m ()
store filehash contents = do
  s <- getStore
  liftIO $ H.insert (unObjectStore s) filehash contents


storeFile :: (MonadIO m, HasLogger m, HasStore m) => Tree -> RawFilePath -> m FileHash
storeFile workingTree filepath = do
  fileBytes <- liftIO $ RFP.readFile filepath
  let fileHash = hashBytes fileBytes
  --info $ "STORE: " <> renderFileHash fileHash <> " " <> T.decodeUtf8 filepath
  store fileHash fileBytes
  let (_, filename) = splitFilepath filepath
  liftIO $ H.insert (unTree workingTree) filename (ContentFile fileHash)
  return fileHash

storeDir :: (MonadIO m, HasLogger m, HasStore m) => Tree -> Tree -> RawFilePath -> m ()
storeDir workingTree subtree filepath = do
  --info $ "STOREDIR: " <> T.decodeUtf8 filepath
  let (_, filename) = splitFilepath filepath
  liftIO $ H.insert (unTree workingTree) filename (ContentTree subtree)
