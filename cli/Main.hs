
import Control.Monad (forever)
import           Options.Applicative
import           Data.Monoid                    ( (<>) )
import           Data.Aeson
import           GHC.Generics
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import           Network
import           Network.Socket                 ( socket
                                                , Family(AF_UNIX)
                                                , SocketType(SeqPacket)
                                                , defaultProtocol
                                                , connect
                                                , SockAddr(SockAddrUnix)
                                                , close
                                                )
import           Network.Socket.ByteString.Lazy (send, recv)
import           Control.Exception
import UnliftIO.Async
import LibWormhole


postMessage :: Socket -> Cmd -> IO ()
postMessage sock msg = do
  let messageBS = encode msg
  sentCount <- send sock messageBS
  putStrLn $ "Sent " <> show sentCount <> " bytes"



cmd :: Parser Cmd
cmd = hsubparser $ command "tree" $ info (pure TreeCmd)
                                         (progDesc "Show the project tree")

openSocket :: IO Socket
openSocket = do
  sock <- socket AF_UNIX SeqPacket defaultProtocol
  connect sock $ SockAddrUnix "/tmp/mysock"
  return sock

readThread :: Socket -> IO ()
readThread sock = forever $ do
  msg <- recv sock 1024
  LBS.putStrLn msg
  

main :: IO ()
main = do
  opts <- execParser (info cmd $ progDesc "~ Unfinished Project ~")
  bracket openSocket close $ \sock -> do
    readAsync <- async $ readThread sock
    print opts
    postMessage sock opts
    wait readAsync

