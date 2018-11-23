module Utils(
    module Utils
) where

import qualified Data.ByteString               as BS
                                         hiding ( hPutStrLn
                                                , putStrLn
                                                , unpack
                                                , pack
                                                )
import qualified Data.ByteString.Char8         as BS
import qualified Data.Text                     as T

showT :: Show a => a -> T.Text
showT = T.pack . show

showBS :: Show a => a -> BS.ByteString
showBS = BS.pack . show


whenM :: Monad m => m Bool -> m () -> m ()
whenM test action = test >>= \case
  True  -> action
  False -> return ()

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM test action = test >>= \case
  True  -> return ()
  False -> action