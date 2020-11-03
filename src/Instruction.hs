{-# LANGUAGE DeriveDataTypeable #-}
module Instruction where

import Data.Char
import Data.Data
import Data.Typeable

-- TODO:
-- store, storer
data Instruction =
  None
  | Loadc Int
  | Dup
  | Pop
  | Add
  | Sub
  | Div
  | Mul
  | Eq
  | Leq
  | Not
  | Printint
  | Jump
  | Jumpz
  | Load Int
  | Store
  | Slide Int
  | Loadsp
  | Loadfp
  | Storefp
  | Loadr Int
  | Storer
  | Halt
  deriving (Read,
            Eq,
            Typeable,
            Data) -- need that to show instructions in lowercase

instance Show Instruction where
  show  = showInstr

showInstr :: Instruction -> String
showInstr (Loadc v) = "loadc " ++ show v -- special case
showInstr (Load v)  = "load "  ++ show v
showInstr (Slide v) = "slide " ++ show v
showInstr (Loadr v) = "loadr " ++ show v
showInstr i = showInstr' i
  where showInstr' instr = map toLower . showConstr $ toConstr instr
