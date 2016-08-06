module SLModel exposing (..)

import Time exposing (Time)
import Char exposing (isDigit)
import Combine exposing (..)
import Combine.Char exposing (char, space, satisfy, eol, noneOf)
import Combine.Infix exposing (..)
import Combine.Num exposing (..)
import String


type alias ServerLog =
    List SLMessage


type
    SLMessage
    --          Time LogLevel Logger Thread Payload
    = SLMessage Time LogLevel String String String


type LogLevel
    = TRACE
    | DEBUG
    | INFO
    | WARN
    | ERROR
    | FATAL


parseServerLog : String -> Result String ServerLog
parseServerLog serverLogText =
    -- TODO ugly - appending line to force last message to be parseds
    case parse serverLogP serverLogText of
        ( Ok sl, _ ) ->
            Ok sl

        ( Err errors, { input } ) ->
            Err
                <| "Parsing server.log failed:\n"
                ++ String.join "\n" errors
                ++ String.left 1000 input


serverLogP : Parser ServerLog
serverLogP =
    slMessageP `manyTill` end


slMessageP : Parser SLMessage
slMessageP =
    SLMessage
        <$> (timeP <* space)
        <*> (logLevelP <* many1 space)
        <*> (loggerP <* space)
        <*> (threadP <* space)
        <*> payloadP



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
    let
        otherLineP =
            (eol *> succeed "")
                `or` ((\fstChr restLine -> String.cons fstChr restLine)
                        <$> satisfy (not << Char.isDigit)
                        <*> restOfLineP
                     )

        restOfLineP =
            while ((/=) '\n') <* eol
    in
        (\fstLine otherLines -> String.join "\n" <| fstLine :: otherLines)
            <$> restOfLineP
            <*> many otherLineP
