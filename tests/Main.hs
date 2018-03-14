module Main where

import Test.Tasty (testGroup)
import Data.List (isInfixOf)
import qualified Data.ByteString.Lazy.Char8 as LB
import SLP

entry :: LogEntry
entry = LogEntry {
  entryIP = IP 127 0 0 1
  , entryUser = UserEntry "peter"
  , entryTime = theTime
  , entryRequest = "GET /sample-image.png HTTP/2"
  }

tinyFilePath = "etc/tiny.log"

testTinyLog = testCase "Simple Log Parser tiny test" url $ do
    res <- parseOnly logFileParser tinyFilePath
    assert "Is what it is" $ res == entry

tests = testGroup "tests" [testTinyLog]

main = defaultMain tests


-- put this in a test
-- print $ parseOnly logEntryParser "127.0.0.1 peter [09/02/2018:10:34:12] \"GET /sample-image.png HTTP/2\"" 

