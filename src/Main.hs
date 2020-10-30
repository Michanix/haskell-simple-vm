module Main where

import System.Environment
import System.IO

main :: IO ()
main = do
  (a:args:_) <- getArgs
  case isValidFlag a of
    Just True -> do
      content <- readFile args
      print content
    Nothing   -> error "Wrong flag. Try with -v flag."

isValidFlag :: String -> Maybe Bool
isValidFlag "-v" = Just True
isValidFlag _    = Nothing
