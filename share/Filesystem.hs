module Filesystem(
    (</>)
) where
import           RawFilePath.Directory
import qualified Data.ByteString.UTF8          as UTF8
import qualified System.FilePath as FP

infixr 5 </>

(</>) :: RawFilePath -> RawFilePath -> RawFilePath
a </> b = UTF8.fromString $ (UTF8.toString a) FP.</> (UTF8.toString b)