module Main where

import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  withFile (args!!0) ReadMode (\h -> do
      content <- hGetContents h
      putStr content)
