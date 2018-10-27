
module LibWormhole where

import qualified Data.Text                     as T
import           Control.Monad.IO.Class
import           Data.Aeson
import           GHC.Generics
import           Data.Monoid
import           Data.ByteString               as BS
import           System.Posix.ByteString.FilePath
import           System.Posix.Directory.ByteString
import Data.Word

data Cmd = TreeCmd | DumpCmd deriving (Show, Generic)

instance FromJSON Cmd
instance ToJSON Cmd

data ClientMsg = ClientMsg {
    cmsgCwd :: T.Text,
    cmsgCmd :: Cmd
} deriving (Show, Generic)

instance FromJSON ClientMsg
instance ToJSON ClientMsg

makePathAbsolute :: MonadIO m => RawFilePath -> m RawFilePath
makePathAbsolute filepath = do
    if BS.take 1 filepath == "/"
        then return filepath
        else do
            let (first, rest) = BS.splitAt 3 filepath
            if first == "../"
                then makePathAbsolute rest
                else do
                    cwd <- liftIO $ getWorkingDirectory
                    return $ cwd <> "/" <> filepath

pathSeperator :: Word8
pathSeperator = 47

splitFilepath :: RawFilePath -> (RawFilePath, RawFilePath)
splitFilepath filepath = (BS.init dirpath, filename)
    where (dirpath, filename) = breakEnd (==pathSeperator) filepath
