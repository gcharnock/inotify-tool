{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
module Logger where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

class Loggable a where
    toText :: a -> T.Text

instance Loggable T.Text where
    toText = id

instance Loggable BS.ByteString where 
    toText = T.decodeUtf8

data LogList = LogListNil | forall a. Loggable a => a `LogListCons` LogList

instance Loggable LogList where
    toText LogListNil = ""
    toText (a `LogListCons` r) = toText a <> toText r

infixr 9 `LogListCons`

(~~) :: forall a. Loggable a => a -> LogList -> LogList
(~~) = LogListCons

infixr 9 ~~


data LogList' = LogListNil' | forall a. Loggable a => LogList' `LogListCons'` a

instance Loggable LogList' where
    toText LogListNil' = ""
    toText (a `LogListCons'` r) = toText a <> toText r

infixl 9 `LogListCons'`

(<<) :: (Loggable a, Loggable b) => a -> b -> LogList'
a << b = LogListNil' `LogListCons'` a `LogListCons'` b

infixl 8 <<

(<<<) :: Loggable a => LogList' -> a -> LogList'
l <<< a = l `LogListCons'` a 

infixl 7 <<<

class HasLogger m where
    info :: Loggable a => a -> m ()




example :: (Monad m, HasLogger m) => m ()
example = do
    let bs = "hello" :: BS.ByteString
    let text = " world" :: T.Text
    
    info $ bs << " " <<< bs <<< text

instance HasLogger IO where
    info a = T.putStrLn (toText a)

runExample :: IO ()
runExample = example