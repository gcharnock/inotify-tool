
module UserRequest where

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
import Actions.Directory


class HasUserRequest a where
    getHandle :: a -> Sys.Handle

askHandle :: (MonadReader env m, HasUserRequest env) => m Sys.Handle
askHandle = fmap getHandle ask

data UserReqContext = UserReqContext {
  ucntxHandle :: Sys.Handle,
  ucntxContext :: Context
}

instance HasBroadcast UserReqContext where
    getBroadcast =  cntxINotifyQueue . ucntxContext

instance HasUserRequest UserReqContext where
    getHandle = ucntxHandle

instance HasStore UserReqContext where
    getStore = getStore . ucntxContext

instance HasRootTree UserReqContext where
    getRootTree = getRootTree . ucntxContext 

instance HasStateRoot UserReqContext where
    getStateRoot = getStateRoot . ucntxContext

instance HasLog UserReqContext where
    setLog logger r@UserReqContext { ucntxContext } = 
        r { ucntxContext = ucntxContext { cntxLogger = logger } }
    getLog = getLog . ucntxContext

instance HasINotify UserReqContext where
    getINotify =  getINotify . ucntxContext

type UserRequest env m = (FSWatcher env m, HasUserRequest env)


sendToUser :: UserRequest env m => BS.ByteString -> m ()
sendToUser message = do
  h <- askHandle
  liftIO $ BS.hPutStr h message

printTree :: UserRequest env m => Int -> Tree.Tree -> m ()
printTree indent tree = withRunInIO $ \runInIO ->
  Tree.forM_ tree $ \(filename, contents) -> runInIO $ do
    replicateM_ indent $ sendToUser " "
    sendToUser $ filename <> " -> "
    case contents of
      Tree.ContentFile objectHash ->
        sendToUser $ renderObjectHashBS objectHash <> "\n"
      Tree.ContentTree innerTree -> do
        sendToUser "\n"
        printTree (indent + 2) innerTree


processMessage :: UserRequest env m => ClientMsg -> m ()
processMessage ClientMsg { cmsgCmd, cmsgCwd = Base64JSON cwd } =
  case cmsgCmd of
    TreeCmd -> do
      rootTree <- askRootTree
      printTree 0 rootTree 
    DumpCmd -> do
      rootTree <- askRootTree
      dumpToDirectory rootTree cwd
    CheckoutCmd checkout -> processCheckoutMessage cwd checkout


processCheckoutMessage :: UserRequest evn m => RawFilePath -> Checkout -> m ()
processCheckoutMessage cwd (Checkout checkoutName (Base64JSON checkoutDir)) =
  do
    let absCheckoutDir = cwd </> checkoutDir
    checkoutProject checkoutName absCheckoutDir

clientDecoder :: UserRequest env m => P.Parser BS.ByteString m ()
clientDecoder = P.decodeGet Bin.getWord16be >>= \case
  Left  _             -> error "decode error"
  Right messageLength -> do
    liftIO $ putStrLn "got message length from the socket"
    P.decodeGet (Bin.getByteString (fromIntegral messageLength)) >>= \case
      Left  _         -> error "decode error"
      Right messageBS -> do
        liftIO $ putStrLn "got message from the socket"
        case eitherDecodeStrict messageBS of
          Left errorMsg -> do
            P.lift $ sendToUser $ "was not JSON" <> messageBS
            P.lift $ sendToUser $ BS.pack $ "Error was " <> errorMsg
          Right msg -> do
            liftIO $ putStrLn "decoded correctly"
            P.lift $ processMessage msg
            clientDecoder


clientSocketThread :: (MonadReader Context m, MonadUnliftIO m) => Socket -> m ()
clientSocketThread sock = do
  liftIO $ putStrLn "in read thread"
  hd <- liftIO $ socketToHandle sock Sys.ReadWriteMode
  let fromClient = P.fromHandle hd
  context <- ask
  flip runReaderT (UserReqContext { ucntxHandle = hd, ucntxContext = context }) $ do
    -- P.runEffect $ P.for fromClient (\) >-> toClient
    sendToUser "Hello, you have connected to the socket\n"
    _ <- P.runStateT clientDecoder fromClient
    liftIO $ putStrLn "Read thread exited"

acceptLoop :: (MonadReader Context m, MonadUnliftIO m) => Socket -> m ()
acceptLoop sock = do
  liftIO $ bind sock $ SockAddrUnix "/tmp/mysock"
  liftIO $ listen sock 5
  forever $ do
    liftIO $ putStrLn "waiting to accept"
    (sock', _) <- liftIO $ accept sock
    liftIO $ putStrLn $ "got connection"
    async $ clientSocketThread sock'

