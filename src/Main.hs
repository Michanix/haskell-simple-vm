{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Environment
import System.IO
import Data.Char
import Data.Data
import Data.Typeable
import Control.Monad (forM_)
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Text.Read (readMaybe)

main :: IO ()
main = do
  (a:args:_) <- getArgs
  case isValidFlag a of
    Just True -> do
      content <- readFile args
      let s = initStack
          xs = map words (lines content)
      print xs
    Nothing   -> error "Wrong flag. Try with -v flag."

type FP = Int
type PC = Int

-- increment fp and pc
incC :: Int -> Int
incC = (+1)

incFP' :: State FP FP
incFP' = do
  val <- get
  put (val+1)
  return val

data VM' = VM' { stack :: Stack
             , fp    :: FP
             , pc    :: PC
             , instr :: Instruction
             } deriving (Read)

type VM a = StateT VM' IO a

initVM :: VM'
initVM = VM' {stack=initStack, fp=0, pc=0, instr=None}


--loadInstr :: [String] -> VM ()
loadInstr ::Instruction -> VM ()
loadInstr (Loadc val) = do
  vm <- get
--  maybe (return()) (\x -> modify (push' x)) (Just val)
  modify (push' val)
  liftIO $ print vm -- printing intermediate state

runVM :: IO VM'
--runVM = execStateT (mapM_ (loadInstr) [["loadc", "1"], ["loadc", "2"]]) initVM
runVM = execStateT (mapM_ (loadInstr) [Loadc 1, Loadc 2]) initVM
push' :: Int -> VM' -> VM'
push' x (VM' s fp pc instr) = VM' {stack=(x:s)
                                  , fp=incC fp
                                  , pc=incC fp
                                  , instr=instr
                                  }

-- loadInstr :: Instruction -> VM VM'
-- loadInstr i = do
--   vm <- get
--   case i of
--     None      -> return ()
--     Dup       -> put vm {stack=dup (stack vm),instr=i}
--     (Loadc v) -> put vm {stack=push v (stack vm),instr=i}
--   return vm

instance Show VM' where
  show = showVm

showVm :: VM' -> String
showVm (VM' {stack = s, fp = fp, pc = pc, instr = instr}) =
    "stack: " ++ (show s)  ++ "\n" ++
    "fp: "    ++ (show fp) ++ "\n" ++
    "pc: "    ++ (show pc) ++ "\n" ++
    "instr: " ++ (show instr)

type Stack = [Int]

data Instruction =
  None
  | Loadc Int
  | Dup
  deriving (Read, Typeable, Data) -- need that to show instructions in lowercase

instance Show Instruction where
  show  = showInstr

showInstr :: Instruction -> String
showInstr (Loadc v) = "loadc " ++ (show v)
showInstr instr = showInstr' instr
  where showInstr' instr = map toLower . showConstr $ toConstr instr

initStack :: Stack
initStack = []

dup :: Stack -> Stack
dup (x:xs) = x:x:xs

push :: Int -> Stack -> Stack
push x xs = x:xs

pop :: Stack -> (Maybe Int, Stack)
pop [] = (Nothing, [])
pop (x:xs) = (Just x, xs)

isValidFlag :: String -> Maybe Bool
isValidFlag "-v" = Just True
isValidFlag _    = Nothing
