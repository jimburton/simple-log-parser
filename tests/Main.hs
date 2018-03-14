{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.HUnit 
import Data.Time
import SLP

theTime :: LocalTime
theTime = LocalTime {
  localDay = fromGregorian 2018 2 9
  , localTimeOfDay = TimeOfDay 10 34 12
  }
          
entry :: LogEntry
entry = LogEntry {
  entryIP     = IP 127 0 0 1
  , entryUser = User "peter"
  , entryTime = theTime
  , entryReq  = "GET /sample-image.png HTTP/2"
  }

testTinyLog :: Test
testTinyLog = TestCase $ do
  res <- parseFromFile "etc/tiny.log"
  assertEqual "Is what it is" res [entry]

{-testLog :: Test
testLog = TestCase $ do
  let res = eitherResult $ parseOnly logParser "etc/access.log"
  case res of
    (Left e)  -> assertFailure $ "Parse failed: " ++ show e
    (Right r) -> assertEqual "Is what it is" 1546 (length r)
-}
tests :: Test
tests = TestList [TestLabel "Test tiny.log" testTinyLog]
{-tests = TestList [TestLabel "Test tiny.log" testTinyLog
                 , TestLabel "Test access.log" testLog
                 ] -}

main :: IO ()
main = do _ <- runTestTT tests
          return ()

