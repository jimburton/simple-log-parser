{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.HUnit 
import Data.Time
import Data.Attoparsec.ByteString.Char8
--import Data.ByteString.Internal.ByteString
--import qualified Data.ByteString.Lazy.Char8 as LB
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
  let res = eitherResult $ parse logParser "etc/tiny.log"
  case res of
    (Left e)  -> assertFailure $ "Parse failed: " ++ show e
    (Right r) -> assertEqual "Is what it is" r [entry]

tests :: Test
tests = TestList [TestLabel "Test tiny.log" testTinyLog]

main :: IO ()
main = do _ <- runTestTT tests
          return ()

