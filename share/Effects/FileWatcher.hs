
module Effects.FileWatcher where

import Control.Effect
import Control.Effect.Carrier
import Data.Coerce

data FileEvent dir fp
    = CreatedDir dir fp
    | DeletedDir dir fp
    | CreatedFile dir fp
    | ModifiedFile dir fp
    | DeletedFile dir fp
    deriving (Show)

data FileWatcher dir fp (m :: * -> *) k
    = WatchDir dir fp (FileEvent dir fp -> IO ()) k
    deriving (Functor)

instance HFunctor (FileWatcher dir fp) where
    hmap _ = coerce

type FWatcher dir fp sig m = (Member (FileWatcher dir fp) sig, Carrier sig m)

watchDir :: (FWatcher dir fp sig m) => dir -> fp -> (FileEvent dir fp -> IO ()) -> m ()
watchDir hDir fp callback = send $ WatchDir hDir fp callback $ ret ()