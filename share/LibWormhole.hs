
module LibWormhole where

import qualified Data.Text as T
import Data.Aeson
import GHC.Generics

data Cmd = TreeCmd | DumpCmd deriving (Show, Generic)

instance FromJSON Cmd
instance ToJSON Cmd

data ClientMsg = ClientMsg {
    cmsgCwd :: T.Text,
    cmsgCmd :: Cmd
} deriving (Show, Generic)

instance FromJSON ClientMsg
instance ToJSON ClientMsg
