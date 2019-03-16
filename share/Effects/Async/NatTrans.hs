
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}

module Effects.Async.NatTrans where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Exception (try, SomeException)

import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.Sum

import           Effects.Async

import           Data.Proxy

import           Data.Coerce
import qualified Control.Concurrent.Async as A

newtype ToIO m = ToIO { unToIO :: forall x. m x -> IO x}

newtype NatTransC (n :: * -> *) b (m :: * -> *) (a :: *) = 
  NatTransC { runNatTransC :: ToIO n -> m a }
  deriving (Functor)

instance Applicative m => Applicative (NatTransC n b m) where
  pure a = NatTransC $ \_ -> pure a
  a <*> b = NatTransC $ \i -> (runNatTransC a) i <*> (runNatTransC b) i

instance Monad m => Monad (NatTransC n b m) where
  return = pure
  ma >>= f = NatTransC $ \i -> (runNatTransC ma) i >>= \a -> (runNatTransC (f a)) i


instance (MonadIO m, Carrier sig m) 
         => Carrier (AsyncEff n a :+: sig) (NatTransC n a m) where
  ret a = NatTransC $ \_ -> return a
  eff = eff'

eff' :: (MonadIO m, Carrier sig m) 
      => (AsyncEff n b :+: sig) (NatTransC n b m) (NatTransC n b m a) 
      -> NatTransC n b m a
eff' (R op) = NatTransC $ \nat -> eff (handleReader nat runNatTransC op)        
eff' (L (AsyncEff action k)) = (NatTransC $ \nat ->
                                  fmap (Async :: A.Async a -> Async n a) . liftIO . A.async . unToIO nat $ action) >>= k 
eff' (L (WaitEff (Async async) k)) = (NatTransC $ \nat -> liftIO . A.wait $ async) >>= k 


runNatTrans :: (MonadIO m, Carrier sig m)
           => Proxy b
           -> (forall x. n x -> IO x)
           -> Eff (NatTransC n b m) a -> m a 
runNatTrans _ trans program = runNatTransC (interpret program) (ToIO trans)


