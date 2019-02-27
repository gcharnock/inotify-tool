{-# LANGUAGE ScopedTypeVariables #-}

module Effects.Filesystem where

import Data.Proxy
import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Resource
import Data.Coerce
import Data.ByteString as BS
import System.IO (Handle, IOMode)

data Filesystem fp (m :: * -> *) k
  = OpenFile IOMode fp (Handle -> k)
  | CloseFile (Proxy fp) Handle k
  | ReadFile fp (BS.ByteString -> k)
  | WriteFile fp BS.ByteString k
  | DeleteFile fp k

  | CreateDirectory fp k
  | ListDirectory fp ([fp] -> k)
  | DoesDirectoryExist fp (Bool -> k)
  | RemoveDirectory fp k

  | AppendPath fp fp (fp -> k)

  | GetCWD (fp -> k)
  deriving (Functor)

instance HFunctor (Filesystem fp) where
    hmap _ = coerce

instance Effect (Filesystem fp) where
    handle state handler = \case
         OpenFile ioMode fp k -> OpenFile ioMode fp $ handler . (<$ state) . k
         CloseFile proxy h k -> CloseFile proxy h $ handler $ k <$ state
         ReadFile fp k -> ReadFile fp $ handler . (<$ state) . k
         WriteFile fp contents k -> WriteFile fp contents $ handler $ k <$ state
         DeleteFile fp k -> DeleteFile fp $ handler $ k <$ state

         CreateDirectory fp k -> CreateDirectory fp $ handler $ k <$ state
         ListDirectory fp k -> ListDirectory fp $ handler . (<$ state) . k
         RemoveDirectory fp k -> RemoveDirectory fp $ handler $ k <$ state
         DoesDirectoryExist fp k -> DoesDirectoryExist fp $ handler . (<$ state) . k

         AppendPath fp1 fp2 k -> AppendPath fp1 fp2 $ handler . (<$ state) . k

         GetCWD k -> GetCWD $ handler . (<$ state) . k

type FS fp sig m = (Member (Filesystem fp) sig, Carrier sig m)

openFile :: FS fp sig m => IOMode -> fp -> m Handle
openFile ioMode fp = send $ OpenFile ioMode fp ret

closeFile :: FS fp sig m => Proxy fp -> Handle -> m ()
closeFile proxy h = send $ CloseFile proxy h (ret ())

createDirectory :: FS fp sig m => fp -> m ()
createDirectory fp = send $ CreateDirectory fp $ ret ()

removeDirectory :: FS fp sig m => fp -> m ()
removeDirectory fp = send $ RemoveDirectory fp $ ret ()

readFile :: FS fp sig m => fp -> m ByteString
readFile fp = send $ ReadFile fp ret

writeFile :: FS fp sig m => fp -> ByteString -> m ()
writeFile fp contents = send $ WriteFile fp contents $ ret ()

listDirectory :: FS fp sig m => fp -> m [fp]
listDirectory fp = send $ ListDirectory fp ret

doesDirectoryExist :: FS fp sig m => fp -> m Bool
doesDirectoryExist fp = send $ DoesDirectoryExist fp ret

deleteFile :: FS fp sig m => fp -> m ()
deleteFile fp = send $ DeleteFile fp $ ret ()

getCwd :: FS fp sig m => m fp 
getCwd = send $ GetCWD ret

appendPath :: FS fp sig m => fp -> fp -> m fp
appendPath fp1 fp2 = send $ AppendPath fp1 fp2 ret

withFile :: forall fp sig m a. (Member (Filesystem fp) sig, Member Resource sig, Carrier sig m) 
         => IOMode -> fp -> (Handle -> m a) -> m a
withFile mode fp = let proxy = Proxy :: Proxy fp in
  bracket (openFile mode fp) (closeFile proxy)

