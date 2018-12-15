module Model.ServerLog exposing
    ( AggregatedMessageByPayload(..)
    , LogLevel(..)
    , ServerLog
    , ServerLogMessage(..)
    , ServerLogMessageField(..)
    , aggregateMessagesByPayload
    , fieldToString
    , logLevelToString
    , serverLogDecoder
    )

import Iso8601
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Time exposing (Posix)
import Tuple


type alias ServerLog =
    List ServerLogMessage


type
    ServerLogMessage
    --  Date LogLevel Logger Thread Payload Stacktrace
    = M Posix LogLevel String String String (Maybe String)


type
    AggregatedMessageByPayload
    --   payload, LogLevel, number of times the same message was present in the log
    = AMP String LogLevel Int


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
    Decode.list serverLogMessageDecoder


serverLogMessageDecoder : Decoder ServerLogMessage
serverLogMessageDecoder =
    Decode.map6 M
        (Decode.index 0 Iso8601.decoder)
        (Decode.index 1 logLevelDecoder)
        (Decode.index 2 Decode.string)
        (Decode.index 3 Decode.string)
        (Decode.index 4 Decode.string)
        (Decode.index 5 <| Decode.maybe Decode.string)


logLevelDecoder : Decoder LogLevel
logLevelDecoder =
    Decode.map readLogLevel Decode.string


logLevelToString : LogLevel -> String
logLevelToString ll =
    case ll of
        TRACE ->
            "TRACE"

        DEBUG ->
            "DEBUG"

        INFO ->
            "INFO"

        WARN ->
            "WARN"

        ERROR ->
            "ERROR"

        FATAL ->
            "FATAL"

        UNKNOWN ->
            "UNKNOWN"


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


fieldToString : ServerLogMessageField -> String
fieldToString f =
    case f of
        DateTimeField ->
            "DateTime"

        LogLevelField ->
            "LogLevel"

        LoggerField ->
            "Logger"

        ThreadField ->
            "Thread"

        PayloadField ->
            "Payload"

        StacktraceField ->
            "StrackTrace"


aggregateMessagesByPayload : ServerLog -> List AggregatedMessageByPayload
aggregateMessagesByPayload sl =
    List.map (\(M _ logLevel _ _ payload _) -> ( logLevel, payload )) sl
        |> List.sortBy Tuple.second
        |> List.Extra.groupWhile (\( _, x ) ( _, y ) -> x == y)
        |> List.map
            (\( ( ll, pl ), groupTail ) ->
                AMP pl ll (1 + List.length groupTail)
            )
        |> List.sortBy (\(AMP _ _ count) -> count)
        |> List.reverse
