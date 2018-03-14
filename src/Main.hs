module Main where

import           System.Environment
import           Data.Attoparsec.ByteString.Char8 (parseOnly)
import           SLP

usage :: String
usage = "Usage: slp <logfile>"

main :: IO ()
main = do args <- getArgs
          if length args /= 1 then putStrLn usage
          else do res <- parseFromFile $ head args
                  print res
