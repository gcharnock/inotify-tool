
module Effects.Notice where

import qualified Data.Text as T

import Control.Effect
import Control.Effect.Carrier
import Data.Coerce

data Notice (m :: * -> *) k
    = Notice T.Text k
    deriving (Functor)

instance HFunctor Notice where
    hmap _ = coerce

instance Effect Notice where
    handle state handler = \case
         Notice msg k -> Notice msg $ handler $ k <$ state

type N sig m = (Member Notice sig, Carrier sig m)

sendNotice :: (N sig m) => T.Text -> m ()
sendNotice msg = send $ Notice msg $ ret ()
