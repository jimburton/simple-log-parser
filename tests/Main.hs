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
  entryIP       = IP 127 0 0 1
  , entryClient = NoClient
  , entryUser   = User "peter" 
  , entryTime   = theTime
  , entryReq    = "GET /sample-image.png HTTP/2"
  , entryStatus = Status 200
  , entrySize   = ResponseSize 123456
  }
  
testTinyLog :: Test
testTinyLog = TestCase $ do
  res <- parseFromFile "etc/tiny.log"
  assertEqual "Is what it is" res [entry]

testLog :: Test
testLog = TestCase $ do
  res <- parseFromFile "etc/tiny.log"
  assertEqual "I can't believe I ate the whole thing'" 1546 (length res)

tests :: Test
tests = TestList [TestLabel "Test tiny.log" testTinyLog
                 , TestLabel "Test access.log" testLog
                 ]

main :: IO ()
main = do _ <- runTestTT tests
          return ()

