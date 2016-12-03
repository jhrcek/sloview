{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.Lazy
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (UTCTime), picosecondsToDiffTime)
import Model.ServerLog
import Test.HUnit
import Text.Parsec (parse)
import Text.Parsec.Text.Lazy
import Text.Printf


main :: IO ()
main = do
    runTestTT allTests
    return ()

allTests = TestList
    [ parseTest dateP "2016-01-02" (fromGregorian 2016 1 2)
    , parseTest timeP "01:02:03,004" (picosecondsToDiffTime $ 10^12 * (3600 + 2*60 + 3) + 4*10^9)
    , parseTest dateTimeP "01:02:03,004" (UTCTime (fromGregorian 2000 1 1) $ picosecondsToDiffTime $ 10^12 * (3600 + 2*60 + 3) + 4*10^9)
    , parseTest dateTimeP "2020-10-11 01:02:03,004" (UTCTime (fromGregorian 2020 10 11) $ picosecondsToDiffTime $ 10^12 * (3600 + 2*60 + 3) + 4*10^9)
    , parseTest logLevelP "WARN" WARN
    , parseTest loggerP "[my.logger]" "my.logger"
    , parseTest threadP "(simpleThreadName)" "simpleThreadName"
    , parseTest threadP "(Thread-5 (HornetQ-client-global-threads-242452152))" "Thread-5 HornetQ-client-global-threads-242452152"
    ]

parseTest :: (Show a, Eq a) => Parser a -> Text -> a -> Test
parseTest parser parserInput expectedOutput =
    let
        result = parse parser "" parserInput
        successDescription = printf "Input %s should be parsed as %s" (show parserInput) (show expectedOutput)
        reportParserError err = printf "Input %s should be parsed as %s but was parser error instead: %s" (show parserInput) (show expectedOutput) (show err)
    in case result of
        Left err -> test . assertFailure $ reportParserError err
        Right r  -> successDescription ~: expectedOutput ~?= r
