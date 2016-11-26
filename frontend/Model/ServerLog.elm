module Model.ServerLog exposing (serverLogDecoder, ServerLog, ServerLogMessage(M), LogLevel(..), ServerLogMessageField(..))

import Date exposing (Date, fromTime)
import Json.Decode exposing (..)
import Result


type alias ServerLog =
    List ServerLogMessage


type
    ServerLogMessage
    --  Date LogLevel Logger Thread Payload Stacktrace
    = M Date LogLevel String String String (Maybe String)


type ServerLogMessageField
    = DateTimeField
    | LogLevelField
    | LoggerField
    | ThreadField
    | PayloadField
    | StacktraceField


type LogLevel
    = TRACE
    | DEBUG
    | INFO
    | WARN
    | ERROR
    | FATAL
    | UNKNOWN


serverLogDecoder : Decoder ServerLog
serverLogDecoder =
    list serverLogMessageDecoder


serverLogMessageDecoder : Decoder ServerLogMessage
serverLogMessageDecoder =
    map6 M
        (index 0 date)
        (index 1 logLevelDecoder)
        (index 2 string)
        (index 3 string)
        (index 4 string)
        (index 5 <| maybe string)


date : Decoder Date
date =
    let
        parseDate str =
            Date.fromString str |> Result.withDefault (fromTime 0)
    in
        Json.Decode.map parseDate string


logLevelDecoder : Decoder LogLevel
logLevelDecoder =
    map readLogLevel string


readLogLevel : String -> LogLevel
readLogLevel s =
    case s of
        "TRACE" ->
            TRACE

        "DEBUG" ->
            DEBUG

        "INFO" ->
            INFO

        "WARN" ->
            WARN

        "ERROR" ->
            ERROR

        "FATAL" ->
            FATAL

        _ ->
            UNKNOWN
