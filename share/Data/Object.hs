{-|
Module: Object

Encapluates all details of the content hashing. Should be possible to change hash function 
by only changing this file.
-}
module Data.Object
  ( ObjectHash
  , renderObjectHash
  , renderObjectHashBS
  , hashBytes
  , objectHashBytes
  )
where

import           Data.Hashable
import           Crypto.Hash                    ( Digest
                                                , SHA256(..)
                                                , hashWith
                                                )
import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.ByteArray                as ByteArray
import qualified Data.ByteArray.Encoding       as ByteArray

newtype ObjectHash = ObjectHash { unFileHash :: (Digest SHA256) }
  deriving (Show, Eq)

instance Hashable ObjectHash where
  hashWithSalt salt (ObjectHash a) = hashWithSalt salt $ (ByteArray.convert a :: BS.ByteString)

renderObjectHash :: ObjectHash -> T.Text
renderObjectHash =
  T.decodeUtf8 . ByteArray.convertToBase ByteArray.Base64 . unFileHash

objectHashBytes :: ObjectHash -> BS.ByteString
objectHashBytes = ByteArray.convert . unFileHash

renderObjectHashBS :: ObjectHash -> BS.ByteString
renderObjectHashBS = ByteArray.convertToBase ByteArray.Base64 . unFileHash

hashBytes :: BS.ByteString -> ObjectHash
hashBytes bs = ObjectHash $ hashWith SHA256 bs
