
module Actions.Directory where

import System.IO as Sys
import Filesystem
import qualified Data.Tree as Tree
import Logging.Contextual.BasicScheme
import           Control.Monad.IO.Class
import           Logging.Contextual 
import           RawFilePath.Directory         (RawFilePath)
import qualified RawFilePath.Directory         as RFP
import Control.Monad.Reader.Class
import qualified Data.ByteString.RawFilePath as RFP
import qualified Data.ByteString as BS
import ObjectStore
import Control.Monad.IO.Unlift

dumpToDirectory :: (MonadReader env m, MonadUnliftIO m, HasStore env, HasLog env) => Tree.Tree -> RawFilePath -> m ()
dumpToDirectory tree filepath = Tree.forM_ tree $ \(filename, treeContent) ->
  case treeContent of
    Tree.ContentTree subtree -> do
      let subpath = filepath </> filename
      liftIO $ RFP.createDirectory subpath
      dumpToDirectory subtree subpath
    Tree.ContentFile fileHash -> do
      contents <- retrive fileHash
      [logInfo|OUT: {filename}|]
      liftIO $ RFP.withFile (filepath </> filename) Sys.WriteMode $ \hd ->
        BS.hPut hd contents