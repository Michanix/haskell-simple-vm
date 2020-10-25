module Main where

import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case isValidFlag (head args) of
    Just True -> do
      content <- readFile (args!!1)
      putStr content
    Nothing   -> error "Wrong flag. Try with -v flag."

isValidFlag :: String -> Maybe Bool
isValidFlag "-v" = Just True
isValidFlag _    = Nothing --error "Wrong flag. Try with -v flag."
