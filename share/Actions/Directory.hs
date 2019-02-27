
module Actions.Directory where

import qualified System.IO                     as Sys
import           UnliftIO.Async
import           Control.Monad.IO.Class
import qualified Data.ByteString               as BS
                                         hiding ( hPutStrLn
                                                , putStrLn
                                                , unpack
                                                , pack
                                                )
import qualified Data.ByteString.RawFilePath   as RFP
                                         hiding ( putStrLn )
import qualified Data.ByteString.Char8         as BS
import           Control.Monad
import           Control.Monad.IO.Unlift
import           Network.Socket
import           Data.Aeson
import           LibWormhole
import qualified Pipes                         as P
import qualified Pipes.Binary                  as P
import qualified Pipes.ByteString              as P
import qualified Pipes.Parse                   as P
import qualified Data.Binary.Get               as Bin
import           Object
import qualified Tree
import           ObjectStore
import           Logging.Contextual 
import           Logging.Contextual.BasicScheme 
import           RawFilePath.Directory         (RawFilePath)
import qualified RawFilePath.Directory         as RFP
import           Filesystem                    
import Control.Monad.Reader.Class
import Control.Monad.Reader (runReaderT)
import FileWatcher
import Context

dumpToDirectory :: (MonadReader env m, HasLog env, HasStore env, MonadUnliftIO m) => Tree.Tree -> RawFilePath -> m ()
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