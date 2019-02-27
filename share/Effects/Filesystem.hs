
module Effects.Filesystem where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Resource
import Data.Coerce
import Data.ByteString as BS
import System.IO (Handle, IOMode)

type RawFilePath = BS.ByteString

data Filesystem (m :: * -> *) k
  = OpenFile IOMode RawFilePath (Handle -> k)
  | CloseFile Handle k
  | ReadFile RawFilePath (BS.ByteString -> k)
  | WriteFile RawFilePath BS.ByteString k
  | DeleteFile RawFilePath k

  | CreateDirectory RawFilePath k
  | ListDirectory RawFilePath ([RawFilePath] -> k)
  | DoesDirectoryExist RawFilePath (Bool -> k)
  | RemoveDirectory RawFilePath k

  | GetCWD (RawFilePath -> k)
  deriving (Functor)

instance HFunctor Filesystem where
    hmap _ = coerce

instance Effect Filesystem where
    handle state handler = \case
         OpenFile ioMode fp k -> OpenFile ioMode fp $ handler . (<$ state) . k
         CloseFile h k -> CloseFile h $ handler $ k <$ state
         ReadFile fp k -> ReadFile fp $ handler . (<$ state) . k
         WriteFile fp contents k -> WriteFile fp contents $ handler $ k <$ state
         DeleteFile fp k -> DeleteFile fp $ handler $ k <$ state

         CreateDirectory fp k -> CreateDirectory fp $ handler $ k <$ state
         ListDirectory fp k -> ListDirectory fp $ handler . (<$ state) . k
         RemoveDirectory fp k -> RemoveDirectory fp $ handler $ k <$ state
         DoesDirectoryExist fp k -> DoesDirectoryExist fp $ handler . (<$ state) . k

         GetCWD k -> GetCWD $ handler . (<$ state) . k

type FS sig m = (Member Filesystem sig, Carrier sig m)

openFile :: FS sig m => IOMode -> RawFilePath -> m Handle
openFile ioMode fp = send $ OpenFile ioMode fp ret

closeFile :: FS sig m => Handle -> m ()
closeFile h = send $ CloseFile h (ret ())

createDirectory :: FS sig m => RawFilePath -> m ()
createDirectory fp = send $ CreateDirectory fp $ ret ()

removeDirectory :: FS sig m => RawFilePath -> m ()
removeDirectory fp = send $ RemoveDirectory fp $ ret ()

readFile :: FS sig m => RawFilePath -> m ByteString
readFile fp = send $ ReadFile fp ret

writeFile :: FS sig m => RawFilePath -> ByteString -> m ()
writeFile fp contents = send $ WriteFile fp contents $ ret ()

listDirectory :: FS sig m => RawFilePath -> m [RawFilePath]
listDirectory fp = send $ ListDirectory fp ret

doesDirectoryExist :: FS sig m => RawFilePath -> m Bool
doesDirectoryExist fp = send $ DoesDirectoryExist fp ret

deleteFile :: FS sig m => RawFilePath -> m ()
deleteFile fp = send $ DeleteFile fp $ ret ()

getCwd :: FS sig m => m RawFilePath
getCwd = send $ GetCWD (ret :: FS sig m => RawFilePath -> m RawFilePath)

withFile :: (Member Filesystem sig, Member Resource sig, Carrier sig m) 
         => IOMode -> RawFilePath -> (Handle -> m a) -> m a
withFile mode fp = bracket (openFile mode fp) closeFile

