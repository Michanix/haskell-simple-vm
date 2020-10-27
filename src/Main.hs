{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Environment
import System.IO
import Data.Char
import Data.Data
import Data.Typeable

main :: IO ()
main = do
  (a:args:_) <- getArgs
  case isValidFlag a of
    Just True -> do
      content <- readFile args
      let s = empty
          xs = map words (lines content)
      print xs
    Nothing   -> error "Wrong flag. Try with -v flag."

type FP = Int
type PC = Int

-- increment PC
incPC :: PC -> PC
incPC = (+1)

incFP :: FP -> FP
incFP = (+1)

data VM = VM { stack :: Stack Int
             , fp    :: FP
             , pc    :: PC
             , instr :: Instruction
             }

runInstr :: Instruction -> VM -> VM
-- Loadc v vm = push v (stack vm)
runInstr (Loadc v) (VM s fp pc insrt) = VM { stack = push v s,
                             fp = incFP fp,
                             pc = incPC pc,
                             instr = Loadc v
                           }
runInstr Dup (VM s fp pc instr)      = VM { stack = dup s,
                             fp = incFP fp,
                             pc = incPC pc,
                             instr = Dup
                           }
instance Show VM where
  show = showVm

showVm :: VM -> String
showVm (VM {stack = s, fp = fp, pc = pc, instr = instr}) =
    "stack: " ++ (show s)  ++ "\n" ++
    "fp: "    ++ (show fp) ++ "\n" ++
    "pc: "    ++ (show pc) ++ "\n" ++
    "instr: " ++ (show instr)

data Stack a = Stack [a]

instance Show a => Show (Stack a) where
  show = showStack

showStack :: Show a => Stack a -> String
showStack (Stack a) = show a -- just show values of stack ie []

data Instruction =
  Loadc Int
  | Dup
  deriving (Typeable, Data) -- need that to show instructions in lowercase

instance Show Instruction where
  show  = showInstr

showInstr :: Instruction -> String
showInstr (Loadc v) = "loadc " ++ (show v)
showInstr instr = map toLower . showConstr $ toConstr instr

empty :: Stack a
empty = Stack []

dup :: Stack a -> Stack a
dup (Stack (x:xs)) = Stack (x:x:xs)

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)

isValidFlag :: String -> Maybe Bool
isValidFlag "-v" = Just True
isValidFlag _    = Nothing
