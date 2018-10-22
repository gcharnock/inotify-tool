
module LibWormhole where

import Data.Aeson
import GHC.Generics

data Cmd = TreeCmd deriving (Show, Generic)

instance FromJSON Cmd
instance ToJSON Cmd
