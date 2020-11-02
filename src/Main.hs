module Main where

import System.Environment
import System.Exit
import System.IO
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Char (toUpper, toLower)
import Text.Read (readMaybe)
import Control.Monad.IO.Class (liftIO)

import Instruction
import VM

main :: IO ()
main = getArgs >>= parse >> exit

parse :: [String] -> IO ()
parse ("-v":file:_) = run file
parse (_:file:_)    = wrongFlag
parse []            = noArgs

run :: String -> IO ()
run file = do
  content <- readFile file
  runVM $ (parseContent . processContent) content

wrongFlag = error "Wrong flag. Try with -v flag." >> terminate
noArgs    = error "No arguments given. Usage: -v [filename]" >> terminate

exit = exitSuccess
terminate = exitWith (ExitFailure 1)

stringToInstr :: [String] -> Maybe Instruction
stringToInstr = readMaybe . unwords . map toCap

processContent :: String -> [[String]]
processContent c = map words $ lines c

-- Filtering out everying that is not an instruction
parseContent :: [[String]] -> [Instruction]
parseContent c = filter (/= None) $ map (fromMaybe None . stringToInstr) c

toCap :: String -> String
toCap []     = []
toCap (x:xs) = toUpper x : map toLower xs
