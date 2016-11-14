module SLModel exposing (..)

import Time exposing (Time)
import Char
import Combine exposing (..)
import Combine.Char exposing (char, space, satisfy, eol, noneOf, anyChar)
import Combine.Num exposing (digit)
import Date exposing (Date)


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


type alias Par a =
    {- stateless parser -}
    Parser () a


parseServerLog : String -> ( List String, ServerLog )
parseServerLog serverLogText =
    let
        messageList =
            splitMessages serverLogText
    in
        messageList
            |> List.map (parse slMessageP)
            |> partitionMessageParseResults


slMessageP : Par SLMessage
slMessageP =
    SLMessage
        <$> (maybeDateP <* many space)
        <*> (timeP <* space)
        <*> (logLevelP <* many1 space)
        <*> (loggerP <* space)
        <*> (threadP <* space)
        <*> payloadP


maybeDateP : Par (Maybe Date)
maybeDateP =
    regex "\\d{4}-\\d{2}-\\d{2}"
        |> Combine.maybe
        |> Combine.map (\maybeDateStr -> maybeDateStr |> Maybe.andThen (Date.fromString >> Result.toMaybe))



{- Parse message timestamp like 07:39:02,146 -}


timeP : Par Time
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


logLevelP : Par LogLevel
logLevelP =
    let
        parseAs str ll =
            string str *> succeed ll
    in
        choice
            [ parseAs "TRACE" TRACE
            , parseAs "DEBUG" DEBUG
            , parseAs "INFO" INFO
            , parseAs "WARN" WARN
            , parseAs "ERROR" ERROR
            , parseAs "FATAL" FATAL
            ]
            <?> "Unknown LogLevel"


loggerP : Par String
loggerP =
    String.fromList
        <$> brackets (many1 (satisfy ((/=) ']')))


threadP : Par String
threadP =
    let
        strWithouParens =
            many1 <| noneOf [ '(', ')' ]
    in
        (String.fromList << List.concat)
            <$> -- workaround for thread names like "(Thread-5 (HornetQ-client-global-threads-242452152))"
                parens (many1 (or strWithouParens (parens strWithouParens)))


payloadP : Par String
payloadP =
    -- anything till the end of the SLMessage
    Combine.map String.fromList <| manyTill anyChar end


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


partitionMessageParseResults : List (Result (ParseErr ()) (ParseOk () SLMessage)) -> ( List String, List SLMessage )
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
                    Err ( _, _, errs ) ->
                        ( (String.join "\n" errs) :: es, ms )

                    Ok ( _, _, m ) ->
                        ( es, m :: ms )
