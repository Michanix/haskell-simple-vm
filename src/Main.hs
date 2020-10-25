module Main where

import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  withFile (head args) ReadMode (\h -> do
      content <- hGetContents h
      putStr content)
