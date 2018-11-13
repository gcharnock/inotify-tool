
module LibWormhole where

import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Aeson                    as Aeson
                                                ( Value(..) )
import           GHC.Generics
import           Data.Monoid
import           Data.ByteString               as BS
import           System.Posix.ByteString.FilePath
import           System.Posix.Directory.ByteString
import           Data.Word
import           Data.ByteArray.Encoding        ( convertToBase
                                                , convertFromBase
                                                , Base(..)
                                                )

newtype Base64JSON = Base64JSON BS.ByteString deriving (Show, Generic)

instance ToJSON Base64JSON where
    toJSON (Base64JSON a) = Aeson.String . T.decodeUtf8 . convertToBase Base64 $ a

instance FromJSON Base64JSON where
    parseJSON s@(String a) = do
        let result = convertFromBase Base64 . T.encodeUtf8 $ a
        case result of
            Left _ -> typeMismatch "Base64JSON" s
            Right b -> return $ Base64JSON b
    parseJSON a = typeMismatch "Base64JSON" a

data Checkout = Checkout T.Text Base64JSON deriving (Show, Generic)

instance FromJSON Checkout
instance ToJSON Checkout

data Cmd = TreeCmd
   | DumpCmd
   | CheckoutCmd Checkout deriving (Show, Generic)

instance FromJSON Cmd
instance ToJSON Cmd

data ClientMsg = ClientMsg {
    cmsgCwd :: Base64JSON,
    cmsgCmd :: Cmd
} deriving (Show, Generic)

instance FromJSON ClientMsg
instance ToJSON ClientMsg

makePathAbsolute :: MonadIO m => RawFilePath -> m RawFilePath
makePathAbsolute filepath = if BS.take 1 filepath == "/"
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
    where (dirpath, filename) = breakEnd (== pathSeperator) filepath
