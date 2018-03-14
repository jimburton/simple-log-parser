module Main where

import           System.Environment
import qualified Data.ByteString.Char8 as BC
import           Data.Attoparsec.ByteString.Char8 (parseOnly)
import           SLP

usage :: String
usage = "Usage: slp <logfile>"

main :: IO ()
main = do args <- getArgs
          if length args /= 1 then putStrLn usage
          else do res <- parseFromFile $ head args
                  print res
