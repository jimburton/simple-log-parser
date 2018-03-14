{-# LANGUAGE OverloadedStrings #-}
module SLP where
{-
A parser for simple access log files in the format

"%h %u %t \"%r\""
<host> <user> <time> <request>

Adapted from the example at https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec
-}
import           Prelude               hiding (takeWhile)
import           Data.Word 
import           Data.Time             hiding (parseTime)
import           Data.String.ToString
import           Data.Attoparsec.ByteString.Char8
import           Control.Applicative

-- | Type for IP's.
data IP        = IP Word8 Word8 Word8 Word8 deriving (Show, Eq)
data UserEntry = NoUser | User String deriving (Show, Eq)
type Request   = String

data LogEntry =
  LogEntry { entryIP   :: IP
           , entryUser :: UserEntry 
           , entryTime :: LocalTime
           , entryReq  :: Request
             } deriving (Show, Eq)

type Log = [LogEntry]

-----------------------
------- PARSING -------
-----------------------

-- | Parser of values of type 'IP'.
parseIP :: Parser IP
parseIP = do
  d1 <- decimal
  char '.'
  d2 <- decimal
  char '.'
  d3 <- decimal
  char '.'
  d4 <- decimal
  return $ IP d1 d2 d3 d4

parseUser :: Parser UserEntry
parseUser =
         (char '-' >> return NoUser)
     <|> (takeWhile (not . isSpace) >>= \u -> return $ User $ toString u)

-- | Parser of values of type 'LocalTime'.
parseTime :: Parser LocalTime
parseTime = do
  char '['
  d  <- count 2 digit
  char '/'
  mm <- count 2 digit
  char '/'
  y  <- count 4 digit
  char ':'
  h  <- count 2 digit
  char ':'
  m  <- count 2 digit
  char ':'
  s  <- count 2 digit
  char ']'
  return 
    LocalTime { localDay = fromGregorian (read y) (read mm) (read d)
              , localTimeOfDay = TimeOfDay (read h) (read m) (read s)
                }

parseRequest :: Parser String
parseRequest = do
  char '"'
  req <- many $ noneOf "\""
  char '"'
  return req
  where noneOf :: String -> Parser Char
        noneOf cs = satisfy (`notElem` cs)
  
-- | Parser of log entries.
logEntryParser :: Parser LogEntry
logEntryParser = do
  i <- parseIP
  char ' '
  u <- parseUser
  char ' '
  t <- parseTime
  char ' '
  r <- parseRequest
  return $ LogEntry i u t r 

logParser :: Parser Log
logParser = many $ logEntryParser <* endOfLine
