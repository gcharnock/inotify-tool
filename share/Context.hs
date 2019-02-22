
module Context where

import           System.INotify hiding (withINotify)
import           Control.Concurrent.STM
import qualified Tree
import           ObjectStore
import           Logging.Contextual 
import           RawFilePath.Directory         (RawFilePath)

data Context = Context {
  cntxINotify :: INotify,
  cntxObjectStore :: ObjectStore,
  cntxRootTree :: Tree.Tree,
  cntxStateRoot :: RawFilePath,
  cntxINotifyQueue :: Maybe (TQueue Event),
  cntxLogger :: Logger
}

instance HasStore Context where
    getStore = getStore

instance HasLog Context where
    getLog = cntxLogger
    setLog logger c = c { cntxLogger = logger }
