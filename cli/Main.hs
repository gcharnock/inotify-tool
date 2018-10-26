import           Control.Monad                  ( forever )
import           Options.Applicative
import           Data.Monoid                    ( (<>) )
import           Data.Aeson
import qualified Data.ByteString.Lazy          as LBS
                                         hiding ( putStrLn )
import qualified Data.ByteString.Lazy.Char8    as LBS
                                                ( putStrLn )
import           Network
import           Network.Socket                 ( socket
                                                , Family(AF_UNIX)
                                                , SocketType(Stream)
                                                , defaultProtocol
                                                , connect
                                                , SockAddr(SockAddrUnix)
                                                , close
                                                )
import           Network.Socket.ByteString.Lazy ( send
                                                , recv
                                                )
import           Control.Exception
import           UnliftIO.Async
import           LibWormhole
import qualified Data.Binary.Put               as Bin
import           System.Posix.Directory.ByteString
                                                ( getWorkingDirectory )
import           Data.Text.Encoding             ( decodeUtf8 )


postMessage :: Socket -> ClientMsg -> IO ()
postMessage sock msg = do
  let payload       = (encode msg <> "\n")
  let payloadLength = LBS.length payload
  let messageBS = Bin.runPut $ do
        Bin.putWord16be $ fromIntegral payloadLength
        Bin.putLazyByteString payload
  sentCount <- send sock messageBS
  putStrLn $ "Sent " <> show sentCount <> " bytes"

cmdParser :: Parser Cmd
cmdParser = hsubparser $ mconcat
  [ command "tree" $ info (pure TreeCmd) (progDesc "Show the project tree")
  , command "dump" $ info
    (pure DumpCmd)
    (progDesc "Dump the contents of a project on to the filesystem")
  ]

openSocket :: IO Socket
openSocket = do
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock $ SockAddrUnix "/tmp/mysock"
  return sock

readThread :: Socket -> IO ()
readThread sock = forever $ do
  msg <- recv sock 1024
  LBS.putStrLn msg


main :: IO ()
main = do
  cmd <- execParser (info cmdParser $ progDesc "~ Unfinished Project ~")
  cwd <- getWorkingDirectory
  bracket openSocket close $ \sock -> do
    readAsync <- async $ readThread sock
    postMessage sock $ ClientMsg {cmsgCwd = decodeUtf8 cwd, cmsgCmd = cmd}
    wait readAsync

