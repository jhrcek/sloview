module SLModel exposing (..)

import Time exposing (Time)
import Char exposing (isDigit)
import Combine exposing (..)
import Combine.Char exposing (char, space, satisfy, eol, noneOf, anyChar)
import Combine.Infix exposing (..)
import Combine.Num exposing (..)
import Date exposing (Date)
import String


type alias ServerLog =
    List SLMessage


type
    SLMessage
    --          Date         Time LogLevel Logger Thread Payload
    = SLMessage (Maybe Date) Time LogLevel String String String


type SLMessageField
    = DateField
    | TimeField
    | LogLevelField
    | LoggerField
    | ThreadField
    | PayloadField


type LogLevel
    = TRACE
    | DEBUG
    | INFO
    | WARN
    | ERROR
    | FATAL


parseServerLog : String -> ( List String, ServerLog )
parseServerLog serverLogText =
    let
        messageList =
            splitMessages serverLogText
    in
        partitionMessageParseResults <| List.map (\m -> fst <| parse slMessageP m) messageList


slMessageP : Parser SLMessage
slMessageP =
    SLMessage
        <$> (maybeDateP <* many space)
        <*> (timeP <* space)
        <*> (logLevelP <* many1 space)
        <*> (loggerP <* space)
        <*> (threadP <* space)
        <*> payloadP


maybeDateP : Parser (Maybe Date)
maybeDateP =
    regex "\\d{4}-\\d{2}-\\d{2}"
        |> Combine.maybe
        |> Combine.map (\maybeDateStr -> maybeDateStr `Maybe.andThen` (Date.fromString >> Result.toMaybe))



{- Parse message timestamp like 07:39:02,146 -}


timeP : Parser Time
timeP =
    let
        twoDigits =
            (\a b -> 10 * a + b) <$> digit <*> digit

        threeDigits =
            (\a b c -> 100 * a + 10 * b + c) <$> digit <*> digit <*> digit
    in
        (\h m s ms ->
            (toFloat h * Time.hour)
                + (toFloat m * Time.minute)
                + (toFloat s * Time.second)
                + (toFloat ms * Time.millisecond)
        )
            <$> (twoDigits <* char ':')
            <*> (twoDigits <* char ':')
            <*> (twoDigits <* char ',')
            <*> threeDigits


logLevelP : Parser LogLevel
logLevelP =
    let
        parseAs str ll =
            string str *> succeed ll
    in
        choice
            [ "TRACE" `parseAs` TRACE
            , "DEBUG" `parseAs` DEBUG
            , "INFO" `parseAs` INFO
            , "WARN" `parseAs` WARN
            , "ERROR" `parseAs` ERROR
            , "FATAL" `parseAs` FATAL
            ]
            <?> "Unknown LogLevel"


loggerP : Parser String
loggerP =
    String.fromList
        <$> brackets (many1 (satisfy ((/=) ']')))


threadP : Parser String
threadP =
    let
        strWithouParens =
            many1 <| noneOf [ '(', ')' ]
    in
        (String.fromList << List.concat)
            <$> -- workaround for thread names like "(Thread-5 (HornetQ-client-global-threads-242452152))"
                parens (many1 (strWithouParens `or` (parens strWithouParens)))


payloadP : Parser String
payloadP =
    -- anything till the end of the SLMessage
    Combine.map String.fromList <| anyChar `manyTill` end


splitMessages : String -> List String
splitMessages s =
    let
        accumulateMessages line msgList =
            case msgList of
                [] ->
                    [ line ]

                x :: xs ->
                    if startsWithDigit line then
                        "" :: (line ++ "\n" ++ x) :: xs
                    else
                        (line ++ "\n" ++ x) :: xs

        startsWithDigit =
            Maybe.withDefault False << Maybe.map (\( c, _ ) -> Char.isDigit c) << String.uncons
    in
        Maybe.withDefault [] << List.tail << List.foldr accumulateMessages [] <| String.lines s


partitionMessageParseResults : List (Result (List String) SLMessage) -> ( List String, List SLMessage )
partitionMessageParseResults rs =
    case rs of
        [] ->
            ( [], [] )

        r :: rs ->
            let
                ( es, ms ) =
                    partitionMessageParseResults rs
            in
                case r of
                    Err errs ->
                        ( (String.join "\n" errs) :: es, ms )

                    Ok m ->
                        ( es, m :: ms )
