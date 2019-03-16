
module Effects.Async where

import Control.Effect
import Control.Effect.Carrier
import Data.Coerce

import qualified Control.Concurrent.Async as A

newtype Async (n :: * -> *) a = Async (A.Async a)

data AsyncEff (n :: * -> *) a (m :: * -> *) k
   = AsyncEff (n a) (Async n a -> k)
   | WaitEff (Async n a) (a -> k)
    deriving (Functor)

instance HFunctor (AsyncEff n a) where
    hmap _ = coerce

instance Effect (AsyncEff n a) where
    handle state handler = \case
         AsyncEff action k -> AsyncEff action $ handler . (<$ state) . k
         WaitEff async k -> WaitEff async $ handler . (<$ state) . k

type CAsyncEff n a sig m = (Member (AsyncEff n a) sig, Carrier sig m)

forkEff :: (CAsyncEff n a sig m) => n a -> m (Async n a)
forkEff action = send $ AsyncEff action ret

waitEff :: (CAsyncEff n a sig m) => (Async n a) -> m a
waitEff async = send $ WaitEff async ret
