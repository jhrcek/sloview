module Model.ServerLog exposing (serverLogDecoder, ServerLog, ServerLogMessage(M), LogLevel(..), ServerLogMessageField(..), AggregatedMessageByPayload(AMP), aggregateMessagesByPayload)

import Date exposing (Date, fromTime)
import Json.Decode exposing (..)
import List.Extra
import Result
import Tuple


type alias ServerLog =
    List ServerLogMessage


type
    ServerLogMessage
    --  Date LogLevel Logger Thread Payload Stacktrace
    = M Date LogLevel String String String (Maybe String)


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


aggregateMessagesByPayload : ServerLog -> List AggregatedMessageByPayload
aggregateMessagesByPayload sl =
    List.map (\(M _ logLevel _ _ payload _) -> ( logLevel, payload )) sl
        |> List.sortBy Tuple.second
        |> List.Extra.groupWhile (\( _, x ) ( _, y ) -> x == y)
        |> List.map
            (\gr ->
                let
                    ( ll, pl ) =
                        Maybe.withDefault ( UNKNOWN, "ERROR" ) <| List.head gr
                in
                    AMP pl ll (List.length gr)
            )
        |> List.sortBy (\(AMP _ _ count) -> count)
        |> List.reverse
