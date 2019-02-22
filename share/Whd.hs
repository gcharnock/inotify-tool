module Whd where

import           UnliftIO.Exception
import           Control.Monad.IO.Class
import           Control.Monad
import qualified Data.Text.Encoding            as T
import           UnliftIO.INotify
import           Control.Monad.Trans.Reader
import           Network.Socket
import qualified Tree
import           ObjectStore
import           Logging.Contextual 
import           Utils
import           RawFilePath.Directory         (RawFilePath)
import qualified RawFilePath.Directory         as RFP
import           Filesystem                    
import qualified System.Posix.ByteString       as Posix
import Context
import FileWatcher
import           UserRequest

type App = ReaderT Context IO

getRootTree :: App Tree.Tree
getRootTree = fmap cntxRootTree ask

getStateRoot :: App RawFilePath
getStateRoot = fmap cntxStateRoot ask

runAppStartup :: App ()
runAppStartup = do
  checkoutDir <- getCheckoutDir
  checkouts   <- liftIO $ RFP.listDirectory checkoutDir
  forM_ checkouts $ \filename -> do
    let checkoutName = T.decodeUtf8 filename
    startProjectSync checkoutName $ checkoutDir </> filename




ensureStateDirIsSetup :: App ()
ensureStateDirIsSetup = do
  checkoutDir <- getCheckoutDir
  liftIO $ whenM (not <$> Posix.fileExist checkoutDir) $ Posix.createDirectory
    checkoutDir
    Posix.ownerModes

runApp :: App ()
runApp = do
  liftIO $ RFP.tryRemoveFile "/tmp/mysock"
  ensureStateDirIsSetup
  runAppStartup
  bracket (liftIO $ socket AF_UNIX Stream defaultProtocol)
          (liftIO . close)
          acceptLoop

main :: IO ()
main = do
  let loggerSettings = LoggerSettings {
    lsHostname = "127.0.0.1",
    lsPort = 5432,
    lsUsername = "postgres",
    lsPassword = "",
    lsWriterCount = 1,
    lsDbName = "log"
  }
  withLogger loggerSettings (LogEvent "whd-app" Nothing) $ \logger ->
    flip runReaderT logger $ do
      objectStore <- newInMemoryStore
      rootTree    <- Tree.new
      (liftIO RFP.getHomeDirectory) >>= \case
        Nothing      -> (error "could not get $HOME":: ReaderT Logger IO a)
        Just homeDir -> withINotify $ \inotify -> do   
          let context = Context
                { cntxINotify      = inotify
                , cntxObjectStore  = objectStore
                , cntxRootTree     = rootTree
                , cntxStateRoot    = homeDir <> "/var/wh"
                , cntxINotifyQueue = Nothing
                , cntxLogger       = logger 
                }
          liftIO $ runReaderT runApp context


