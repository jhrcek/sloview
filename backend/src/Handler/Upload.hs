{-# LANGUAGE OverloadedStrings #-}
module Handler.Upload (uploadHandler) where

import Control.Monad (unless)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.Text.Lazy as T (pack)
import Handler.Index as Index
import Model.ServerLog as SL
import Snap.Core (Snap, logError, writeLazyText)
import Snap.Util.FileUploads (PartInfo, PolicyViolationException, UploadPolicy,
                              allowWithMaximumSize, defaultUploadPolicy,
                              handleFileUploads, setMaximumFormInputSize,
                              setMaximumNumberOfFormInputs)
import Text.Parsec (ParseError)

uploadHandler :: Snap ()
uploadHandler = do
    [exOrParseResult] <- handleFileUploads "/tmp" serverLogUploadPolicy --policy should guarrantee only 1 file will be uploaded (?)
        (const $ allowWithMaximumSize maxUploadFileSize)
        handleUpload
    either handleException handleParsedServerLog exOrParseResult

handleUpload :: PartInfo
                -> Either PolicyViolationException FilePath
                -> IO (Either PolicyViolationException (ServerLog, [ParseError]))
handleUpload _pinfo =
    either
        (return . Left) -- just past the exception out
        (fmap Right . SL.parseServerLog)


handleException :: PolicyViolationException -> Snap ()
handleException =
    writeLazyText . T.pack . show

handleParsedServerLog :: (ServerLog, [ParseError]) -> Snap ()
handleParsedServerLog (slog, errors) = do
    logErrors errors
    Index.indexHandler slog

logErrors :: [ParseError] -> Snap ()
logErrors errs =
    unless (null errs) $
        logError . BS.pack . unlines $
        take 11 ("There were some errors when parsing server.log. Showing (at most) the first 10:" : map show errs)


serverLogUploadPolicy :: UploadPolicy
serverLogUploadPolicy =
    setMaximumNumberOfFormInputs 1 $
    setMaximumFormInputSize maxUploadFileSize defaultUploadPolicy

maxUploadFileSize :: Int64
maxUploadFileSize =
    2^(27::Int) --128 Mb
