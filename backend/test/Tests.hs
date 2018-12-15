{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import Data.Text.Lazy (Text)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (DiffTime, UTCTime (UTCTime), picosecondsToDiffTime)
import Model.ServerLog
import Test.HUnit
import Text.Parsec (parse)
import Text.Parsec.Text.Lazy (Parser)
import Text.Printf (printf)


main :: IO ()
main = do
    _ <- runTestTT allTests
    return ()

allTests :: Test
allTests = TestList
    [ parseTest dateP "2016-01-02" (fromGregorian 2016 1 2)
    , parseTest timeP "01:02:03,004" testTime
    , parseTest dateTimeP "01:02:03,004" (UTCTime (fromGregorian 2000 1 1) testTime)
    , parseTest dateTimeP "2020-10-11 01:02:03,004" (UTCTime (fromGregorian 2020 10 11) testTime)
    , parseTest logLevelP "WARN" WARN
    , parseTest loggerP "[my.logger]" "my.logger"
    , parseTest threadP "(simpleThreadName)" "simpleThreadName"
    , parseTest threadP "(Thread-5 (HornetQ-client-global-threads-242452152))" "Thread-5 HornetQ-client-global-threads-242452152"
    ]
    where
      testTime :: DiffTime
      testTime = picosecondsToDiffTime $ 10^(12::Int) * (3600 + 2*60 + 3) + 4*10^(9::Int)

parseTest :: (Show a, Eq a) => Parser a -> Text -> a -> Test
parseTest parser parserInput expectedOutput =
    let
        result = parse parser "" parserInput
        successDescription = printf "Input %s should be parsed as %s" (show parserInput) (show expectedOutput)
        reportParserError err = printf "Input %s should be parsed as %s but was parser error instead: %s" (show parserInput) (show expectedOutput) (show err)
    in case result of
        Left err -> test @Assertion . assertFailure $ reportParserError err
        Right r  -> successDescription ~: expectedOutput ~?= r
