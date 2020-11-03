module VM
  ( VM' (..),
    runVM
  ) where

import Instruction

import System.Exit
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)

type FP = Int
type PC = Int
type Stack = [Int]

initStack :: Stack
initStack = []

-- increment counter
incC :: Int -> Int
incC = (+1)

data VM' = VM' {stack :: Stack
               ,fp    :: FP
               ,pc    :: PC
               ,instr :: Instruction
               } deriving (Read)

instance Show VM' where
  show = showVm

showVm :: VM' -> String
showVm (VM' s fp pc instr) =
    "stack: " ++ show s  ++ "\n" ++
    "fp: "    ++ show fp ++ "\n" ++
    "pc: "    ++ show pc ++ "\n" ++
    "instr: " ++ show instr ++ "\n"

initVM :: VM'
initVM = VM' {stack = initStack, fp = 0, pc = 0, instr = None}

type VM a = StateT VM' IO a

runVM :: [Instruction] -> IO VM'
runVM is = execStateT (mapM runInstr is) initVM

runInstr ::Instruction -> VM VM'
runInstr i = case i of
  (Loadc val) -> f (loadc val)
  Dup         -> f dup
  Pop         -> f pop
  Add         -> f (appBinOp i (+))
  Sub         -> f (appBinOp i (-))
  Div         -> f (appBinOp i div)
  Mul         -> f (appBinOp i (*))
  Eq          -> f (appLogOp i (==))
  Leq         -> f (appLogOp i (<=))
  Not         -> f notOp
  Printint    -> printint
  Jump        -> f jump
  Jumpz       -> f jumpz
  (Load val)  -> f (load val)
  Store       -> undefined -- TODO
  (Slide n)   -> f (slide n)
  Loadsp      -> f loadsp
  Loadfp      -> f loadfp
  Storefp     -> f storefp
  (Loadr n)   -> f (loadr n)
  Storer      -> undefined -- TODO
  Halt        -> liftIO exitSuccess
  where f i = do            -- modifying intermediate state
          vm <- get         -- and printing it
          modify i
          liftIO $ print vm
          return vm
        -- special case
        -- prints stack first value
        printint = get >>= \vm ->
          liftIO $ print (stack vm!!(length (stack vm) - 1)) >>
          return vm

-- VM instructions
-- instructions with argument
loadc :: Int -> VM' -> VM'
loadc x (VM' s fp pc i) = VM' {stack = x:s
                              ,fp    = fp
                              ,pc    = incC pc
                              ,instr = Loadc x
                              }

load :: Int -> VM' -> VM'
load n (VM' (x:xs) fp pc i) = VM' {stack = xs!!n : xs
                                  ,fp    = fp
                                  ,pc    = incC pc
                                  ,instr = Load n
                                  }

slide :: Int -> VM' -> VM'
slide n (VM' (x:xs) fp pc i) = VM' {stack = x : drop n xs
                                   ,fp    = fp
                                   ,pc    = incC pc
                                   ,instr = Slide n
                                   }

loadr :: Int -> VM' -> VM'
loadr n (VM' xs fp pc i) = VM' {stack = xs !! (length xs-1-fp-n):xs
                              ,fp    = fp
                              ,pc    = incC pc
                              ,instr = Loadr n
                              }

-- instructions without argument
dup :: VM' -> VM'
dup (VM' (x:xs) fp pc i) = VM' {stack = x:x:xs
                               ,fp    = fp
                               ,pc    = incC pc
                               ,instr = Dup
                               }

pop :: VM' -> VM'
pop (VM' (x:xs) fp pc i) = VM' {stack = xs
                               ,fp    = fp
                               ,pc    = incC pc
                               ,instr = Pop
                               }

jump :: VM' -> VM'
jump (VM' (x:xs) fp pc i) = VM' {stack = xs
                                ,fp    = fp
                                ,pc    = x
                                ,instr = Jump
                                }

jumpz :: VM' -> VM'
jumpz (VM' (x:0:xs) fp pc i) = VM' {stack = xs
                                ,fp       = fp
                                ,pc       = x
                                ,instr    = Jumpz
                                }
jumpz (VM' (x:1:xs) fp pc i) = VM' {stack = xs
                                ,fp       = fp
                                ,pc       = incC pc
                                ,instr    = Jumpz
                                }

loadsp :: VM' -> VM'
loadsp (VM' xs fp pc i) = VM' {stack = length xs - 1 : xs
                              ,fp    = fp
                              ,pc    = incC pc
                              ,instr = Loadsp
                              }

loadfp :: VM' -> VM'
loadfp (VM' xs fp pc i) = VM' {stack = fp : xs
                              ,fp    = fp
                              ,pc    = incC pc
                              ,instr = Loadfp
                              }

storefp :: VM' -> VM'
storefp (VM' (x:xs) fp pc i) = VM' {stack = xs
                                   ,fp    = x
                                   ,pc    = incC pc
                                   ,instr = Storefp
                                   }

-- Arithmetics
type BinOp = (Int -> Int -> Int)

appBinOp :: Instruction -> BinOp -> VM' -> VM'
appBinOp instr op (VM' (x:y:xs) fp pc i) = VM' {stack  = op y x:xs
                                                ,fp    = fp
                                                ,pc    = incC pc
                                                ,instr = instr
                                                }

-- Logical operations
type LogicalOp = (Int -> Int -> Bool)

appLogOp :: Instruction -> LogicalOp -> VM' -> VM'
appLogOp instr op (VM' (x:y:xs) fp pc i) = VM' {stack = g (op y x):xs
                                               ,fp    = fp
                                               ,pc    = incC pc
                                               ,instr = instr
                                               }
  where g c = fromEnum c -- convert from Bool to Int(ie True -> 1, False -> 0)

notOp :: VM' -> VM'
notOp (VM' (x:xs) fp pc i) = VM' {stack = g x:xs
                                 ,fp    = fp
                                 ,pc    = incC pc
                                 ,instr = Not
                                 }
  where g x = if x == 0 then 1 else 0
