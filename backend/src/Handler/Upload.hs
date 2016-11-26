{-# LANGUAGE OverloadedStrings #-}
module Handler.Upload (uploadHandler) where

import Data.Int (Int64)
import Data.Text.Lazy (pack)
import qualified Data.Text.Lazy.IO as TIO
import Snap.Core (Snap, writeLazyText)
import Snap.Util.FileUploads (handleFileUploads, defaultUploadPolicy, allowWithMaximumSize, setMaximumFormInputSize, UploadPolicy, setMaximumNumberOfFormInputs, PartInfo, PolicyViolationException)

import Model.ServerLog as SL
import Handler.Index as Index

uploadHandler :: Snap ()
uploadHandler = do
    [exOrText] <- handleFileUploads "/tmp" serverLogUploadPolicy --policy should guarrantee only 1 file will be uploaded (?)
        (const $ allowWithMaximumSize maxUploadFileSize)
        handleUpload
    either handleException handleParsedServerLog exOrText

handleUpload :: PartInfo -> Either PolicyViolationException FilePath -> IO (Either PolicyViolationException ServerLog)
handleUpload _pinfo = either
    (return . Left) -- just past the exception out
    (\f -> Right . SL.parseServerLogText <$> TIO.readFile f)

handleException :: PolicyViolationException -> Snap ()
handleException = writeLazyText . pack . show

handleParsedServerLog :: ServerLog -> Snap ()
handleParsedServerLog = Index.indexHandler

serverLogUploadPolicy :: UploadPolicy
serverLogUploadPolicy =
    setMaximumNumberOfFormInputs 1 $
    setMaximumFormInputSize maxUploadFileSize defaultUploadPolicy

maxUploadFileSize :: Int64
maxUploadFileSize = 2^(20::Int) --1 Mb
