module Main where

import System.Environment
import System.Exit
import System.IO
import Data.Maybe (fromMaybe)
import Data.Char (toUpper, toLower)
import Text.Read (readMaybe)

import Instruction
import VM

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs ("-v":file:_) = run file  >>= print  >> exit
parseArgs (_:file:_)    = wrongFlag >> terminate
parseArgs []            = noArgs    >> terminate

-- run app
run :: String -> IO VM'
run file = do
  content <- readFile file
  runVM $ (parseContent . processContent) content

-- to ensure that program terminates
exit = exitSuccess
terminate = exitWith (ExitFailure 1)
-- helpers
wrongFlag = error "Wrong flag. Try with -v flag."            >> terminate
noArgs    = error "No arguments given. Usage: -v [filename]" >> terminate

-- reading file content and returning as nested list of strings
-- where each list is representing content of the each line
processContent :: String -> [[String]]
processContent = map words . lines

-- reads list of strings and translates them into instructions
stringToInstr :: [String] -> Maybe Instruction
stringToInstr = readMaybe . unwords . map toCap
  -- function to help tranlate strings into Instructions
  where toCap []     = []
        toCap (x:xs) = toUpper x : map toLower xs

-- returns None if something unknown and after filtering instruction None
-- return list of defined instructions
parseContent :: [[String]] -> [Instruction]
parseContent = filter (/= None) . map (fromMaybe None . stringToInstr)
