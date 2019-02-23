
module Data.Tree
  ( Tree
  , TreeContent(..)
  , add
  , addSubTree
  , forM_
  , new
  , remove
  , get
  , getLength
  , lookup
  )
where

import qualified Control.Monad as M
import           Control.Monad.IO.Unlift
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashTable.IO             as H
import           Data.Object
import           RawFilePath.Directory as RFP (RawFilePath)
import           Data.Binary.Put

type HashTable k v = H.BasicHashTable k v

data TreeContent = ContentFile ObjectHash | ContentTree Tree
  deriving Show


newtype Tree = Tree { unTree :: HashTable RawFilePath TreeContent } deriving (Show)

add :: MonadIO m => Tree -> RawFilePath -> ObjectHash -> m ()
add tree filename fileHash =
  liftIO $ H.insert (unTree tree) filename (ContentFile fileHash)

addSubTree :: MonadIO m => Tree -> RawFilePath -> Tree -> m ()
addSubTree tree filename subtree =
  liftIO $ H.insert (unTree tree) filename (ContentTree subtree)

remove :: MonadIO m => Tree -> RawFilePath -> m ()
remove tree filePath = liftIO $ H.delete (unTree tree) filePath

new :: MonadIO m => m Tree
new = liftIO $ Tree <$> H.new

getLength :: MonadIO m => Tree -> m Int
getLength tree = liftIO $ fmap length $ H.toList $ unTree tree

get :: MonadIO m => Tree -> RawFilePath -> m (Maybe TreeContent)
get tree filename = liftIO $ H.lookup (unTree tree) filename 

forM_
  :: MonadUnliftIO m => Tree -> ((RawFilePath, TreeContent) -> m ()) -> m ()
forM_ tree action =
  withRunInIO $ \runInIO -> H.mapM_ (\a -> runInIO $ action a) (unTree tree)

treeToObject :: MonadUnliftIO m => Tree -> m LBS.ByteString
treeToObject tree = do
  asList <- liftIO $ H.toList (unTree tree)
  return $ runPut $ do
    let l = length asList
    putWord32be (fromIntegral l)
    M.forM_ asList $ \(filepath, content) -> do
      let lfLen = BS.length filepath
      putWord16be (fromIntegral lfLen)
      putByteString filepath
      case content of
        ContentFile o -> do
          let objectHash = objectHashBytes o
          putWord8 0
          putByteString objectHash
        ContentTree _ -> error "not supported yet"