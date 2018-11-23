
module Tree
(Tree, TreeContent(..), addToTree, addDirToTree, forTree, newTree, removeFromTree)
where

import Control.Monad.IO.Unlift
import qualified Data.HashTable.IO as H
import Object
import Filesystem

type HashTable k v = H.BasicHashTable k v

data TreeContent = ContentFile ObjectHash | ContentTree Tree
  deriving Show


newtype Tree = Tree { unTree :: HashTable RawFilePath TreeContent } deriving (Show)

addToTree :: MonadIO m => Tree -> RawFilePath -> ObjectHash -> m ()
addToTree tree filename fileHash = liftIO $ H.insert (unTree tree) filename (ContentFile fileHash)

addDirToTree :: MonadIO m => Tree -> RawFilePath -> Tree -> m ()
addDirToTree tree filename subtree = liftIO $ H.insert (unTree tree) filename (ContentTree subtree)

removeFromTree :: MonadIO m => Tree -> RawFilePath -> m ()
removeFromTree tree filePath = liftIO $ H.delete (unTree tree) filePath

newTree :: MonadIO m => m Tree
newTree = liftIO $ Tree <$> H.new

forTree
  :: MonadUnliftIO m => Tree -> ((RawFilePath, TreeContent) -> m ()) -> m ()
forTree tree action =
  withRunInIO $ \runInIO -> H.mapM_ (\a -> runInIO $ action a) (unTree tree)