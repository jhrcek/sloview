module Handler.Upload (handler) where

import Data.Int (Int64)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Snap.Core (Snap, writeLazyText)
import Snap.Util.FileUploads (handleFileUploads, defaultUploadPolicy, allowWithMaximumSize, setMaximumFormInputSize, UploadPolicy, setMaximumNumberOfFormInputs, PartInfo, PolicyViolationException)


handler :: Snap ()
handler = do
    [info] <- handleFileUploads "/tmp" serverLogUploadPolicy --policy should guarrantee only 1 file will be uploaded (?)
        (const $ allowWithMaximumSize maxUploadFileSize)
        handleFile
    writeLazyText info

handleFile :: PartInfo -> Either PolicyViolationException FilePath -> IO T.Text
handleFile _pinfo =
    either
        (\exception -> return . T.pack $ show exception)
        (\fpath -> T.take 1000 `fmap` TIO.readFile fpath)

serverLogUploadPolicy :: UploadPolicy
serverLogUploadPolicy =
    setMaximumNumberOfFormInputs 1 $
    setMaximumFormInputSize maxUploadFileSize defaultUploadPolicy

maxUploadFileSize :: Int64
maxUploadFileSize = 2^(20::Int) --1 Mb
