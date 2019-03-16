{-# LANGUAGE ExistentialQuantification #-}

module Effects.Async where

import Control.Effect
import Control.Effect.Carrier
import Data.Coerce

import qualified Control.Concurrent.Async as A

newtype Async (n :: * -> *) a = Async (A.Async a)

data AsyncEff (n :: * -> *) (m :: * -> *) k
   = forall a. AsyncEff (n a) (Async n a -> k)
   | forall a. WaitEff (Async n a) (a -> k)

instance Functor (AsyncEff n m) where
  fmap f (AsyncEff n k) = AsyncEff n (fmap f k)
  fmap f (WaitEff n k) = WaitEff n (fmap f k)

instance HFunctor (AsyncEff n) where
    hmap _ = coerce

instance Effect (AsyncEff n) where
    handle state handler = \case
         AsyncEff action k -> AsyncEff action $ handler . (<$ state) . k
         WaitEff async k -> WaitEff async $ handler . (<$ state) . k

type CAsyncEff n sig m = (Member (AsyncEff n) sig, Carrier sig m)

forkEff :: (CAsyncEff n sig m) => n a -> m (Async n a)
forkEff action = send $ AsyncEff action ret

waitEff :: (CAsyncEff n sig m) => (Async n a) -> m a
waitEff async = send $ WaitEff async ret
