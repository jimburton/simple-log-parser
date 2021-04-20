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
import qualified Data.ByteString.Char8 as BC

-- | Type for IPs.
data IP        = IP Word8 Word8 Word8 Word8 deriving (Show, Eq)
-- | Type for months
data Month = Jan | Feb | Mar | Apr | May | Jun | Jul
           | Aug | Sep | Oct | Nov | Dec deriving (Enum, Show, Read, Eq)
-- | Type for User IDs.
data UserEntry = NoUser | User String deriving (Show, Eq)
-- | Type alias for requests.
type Request   = String
-- | Type for log entries.
data LogEntry =
  LogEntry { entryIP   :: IP
           , entryUser :: UserEntry 
           , entryTime :: LocalTime
           , entryReq  :: Request
             } deriving (Show, Eq)
-- | Type for an entire log.
type Log = [LogEntry]

-----------------------
------- PARSING -------
-----------------------

-- | Helper function.
noneOf :: String -> Parser Char
noneOf cs = satisfy (`notElem` cs)

-- | Parser of values of type 'IP'.
parseIP :: Parser IP
parseIP = do
  d1 <- decimal
  char '.'
  d2 <- decimal
  char '.'
  d3 <- decimal
  char '.'
  IP d1 d2 d3 <$> decimal

-- | Parser of values of type `UserEntry'.
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
  mm <- fmap monthIndex $ some $ noneOf "/"
  char '/'
  y  <- count 4 digit
  char ':'
  h  <- count 2 digit
  char ':'
  m  <- count 2 digit
  char ':'
  s  <- count 2 digit
  many $ noneOf "]"
  char ']'
  return 
    LocalTime { localDay = fromGregorian (read y) mm (read d)
              , localTimeOfDay = TimeOfDay (read h) (read m) (read s)
                }
    where monthIndex :: String -> Int
          monthIndex str = 1 + fromEnum (read str ::Month)


-- | Parser of values of type 'Request'.
parseRequest :: Parser Request
parseRequest = do
  char '"'
  req <- many $ noneOf "\""
  char '"'
  return req
  
-- | Parser of individual log entries.
logEntryParser :: Parser LogEntry
logEntryParser = do
  i <- parseIP
  char ' '
  u <- parseUser
  char ' '
  t <- parseTime
  char ' '
  LogEntry i u t <$> parseRequest 

-- | Parser of an entire log.
logParser :: Parser Log
logParser = many $ logEntryParser <* endOfLine

-- | Parse a log file given the path
parseFromFile :: FilePath -> IO Log
parseFromFile f = do
  contents <- BC.readFile f 
  let res = parseOnly logParser contents
  case res of
    (Left _)  -> return []
    (Right r) -> return r
