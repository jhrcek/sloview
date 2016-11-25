{-# LANGUAGE DeriveGeneric #-}
module Model.ServerLog where

import Data.Aeson (ToJSON, toJSON, Value(Array))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(UTCTime), secondsToDiffTime)
import GHC.Generics
import GHC.Exts (fromList)

type ServerLog = [ServerLogMessage]

data ServerLogMessage = M
    { date :: UTCTime
    , logLevel :: LogLevel
    , logger :: String
    , thread :: String
    , payload :: String
    , exception :: Maybe String
    } deriving (Show, Generic)

instance ToJSON ServerLogMessage where
    toJSON M{date=d, logLevel=ll, logger=l, thread=t, payload=p, exception=e} =
        Array $ fromList [toJSON d, toJSON ll, toJSON l, toJSON t, toJSON p, toJSON e]

data LogLevel
    = TRACE
    | DEBUG
    | INFO
    | WARN
    | ERROR
    | FATAL
    deriving (Show, Generic)

instance ToJSON LogLevel where


testMessage :: ServerLogMessage
testMessage = M
    { date = UTCTime (fromGregorian 2016 11 25) (secondsToDiffTime $ 5 * 3600)
    , logLevel = ERROR
    , logger = "org.some.Logger"
    , thread = "some thread"
    , payload = "Something happened"
    , exception = Nothing
    }
