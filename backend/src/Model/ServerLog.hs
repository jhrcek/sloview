{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.ServerLog where

import Control.Monad (replicateM)
import Data.Aeson (ToJSON, Value (Array), toJSON)
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.Either (partitionEithers)
import Data.Functor (($>))
import Data.Text.Lazy (Text, breakOn, concat, intercalate, lines, null, pack,
                       uncons)
import qualified Data.Text.Lazy.IO as TIO
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (DiffTime, UTCTime (UTCTime), picosecondsToDiffTime)
import Data.Tuple (swap)
import GHC.Exts (fromList)
import GHC.Generics
import Prelude hiding (concat, lines, null, unlines)
import Text.Parsec (ParseError, parse, try, (<|>))
import Text.Parsec.Char (anyChar, char, digit, noneOf, satisfy, space, spaces,
                         string)
import Text.Parsec.Combinator (between, choice, eof, many1, manyTill)
import Text.Parsec.Error (Message (Message), addErrorMessage, newErrorMessage)
import Text.Parsec.Pos (newPos)
import Text.Parsec.Text.Lazy (Parser)

type ServerLog = [ServerLogMessage]

data ServerLogMessage = M
    { date       :: UTCTime
    , logLevel   :: LogLevel
    , logger     :: Text
    , thread     :: Text
    , payload    :: Text
    , stacktrace :: Maybe Text
    } deriving (Show, Generic)

instance ToJSON ServerLogMessage where
    toJSON M{date=d, logLevel=ll, logger=l, thread=t, payload=p, stacktrace=e} =
        Array $ fromList [toJSON d, toJSON ll, toJSON l, toJSON t, toJSON p, toJSON e]

data LogLevel
    = TRACE
    | DEBUG
    | INFO
    | WARN
    | ERROR
    | FATAL
    deriving (Eq, Generic, Show)

instance ToJSON LogLevel where

parseServerLog :: FilePath -> IO (ServerLog, [ParseError])
parseServerLog fpath = parseServerLogText <$> TIO.readFile fpath
  where
    parseServerLogText :: Text -> (ServerLog, [ParseError])
    parseServerLogText = let procError e = ([], [mkParseError e])
        in either procError parseMessages . splitIntoMessages

    parseMessages :: [Text] -> (ServerLog, [ParseError])
    parseMessages = swap . partitionEithers . map parseMessage

    parseMessage :: Text -> Either ParseError ServerLogMessage
    parseMessage msg = first (includeParserInputInError msg) $ parse serverLogMessageP fpath msg

    includeParserInputInError :: Text -> ParseError -> ParseError
    includeParserInputInError input = addErrorMessage (Message $ "in message : " ++ show input)

    mkParseError :: String -> ParseError
    mkParseError str = newErrorMessage (Message str) (newPos "dummy" 1 1)

splitIntoMessages :: Text -> Either String [Text]
splitIntoMessages serverLogText =
  let
    msgList = map (intercalate "\n") . foldr accumulateMessages [] $ lines serverLogText

    accumulateMessages line acc = case acc of
        [] -> [[line]]
        x:xs -> if startsWithDigit line then
                    [] : (line:x) : xs
                else
                    (line:x) : xs

    startsWithDigit = maybe False (isDigit . fst) . uncons

  in case msgList of
      [] -> Left "There were no data to parse"
      --drop first empty msg that's appended there by '[] : (line:x) : xs'^
      (throwawayEmpty:nonemptyMsgs)
          | null throwawayEmpty -> Right nonemptyMsgs
          | otherwise -> Left "There was no message starting with digit"

serverLogMessageP :: Parser ServerLogMessage
serverLogMessageP = (\d ll l t (p,e) ->  M d ll l t p e)
    <$> (dateTimeP  <* space)
    <*> (logLevelP <* spaces)
    <*> (loggerP <* space)
    <*> (threadP <* space)
    <*> payloadP

dateTimeP :: Parser UTCTime
dateTimeP = UTCTime <$> tryDate <*> timeP
  where -- if date not present just return arbitrary fixed date
    tryDate = try (dateP <* space) <|> return fixedDate
    fixedDate = fromGregorian 2000 1 1

dateP :: Parser Day
dateP = fromGregorian -- (Integer -> Int -> Int -> Day)
    <$> (digits 4 <* char '-')
    <*> (fromIntegral <$> digits 2 <* char '-')
    <*> (fromIntegral <$> digits 2)


timeP :: Parser DiffTime
timeP =
  let picosPerSec = 10^(12::Integer)
  in
    (\h m s ms -> picosecondsToDiffTime
      $ (h * 3600 * picosPerSec)
      + (m * 60 * picosPerSec)
      + (s * picosPerSec)
      + (ms * 10^(9::Integer))
    ) <$> (digits 2 <* char ':')
      <*> (digits 2 <* char ':')
      <*> (digits 2 <* char ',')
      <*> digits 3

digits :: Int -> Parser Integer
digits cnt = read <$> replicateM cnt digit

logLevelP :: Parser LogLevel
logLevelP = choice $ map constParser [TRACE, DEBUG, INFO, WARN, ERROR, FATAL]
  where
    constParser c = string (show c) $> c

loggerP :: Parser Text
loggerP = between (char '[') (char ']') (pack <$> many1 (satisfy (']'/=)))

threadP :: Parser Text
threadP =
  let
    strWithouParens =  many1 $ noneOf "()"
    parens = between (char '(') (char ')')
  in -- workaround for thread names like "(Thread-5 (HornetQ-client-global-threads-242452152))"
    parens $ concat . map pack <$> many1 (strWithouParens <|> parens strWithouParens)

payloadP :: Parser (Text, Maybe Text)
payloadP = do
    restOfMessage <- pack <$> manyTill anyChar eof
    let (payload', st) = breakOn "\tat" restOfMessage
        stacktrace' = if null st then Nothing else Just st
    return (payload', stacktrace')
