module VM
  ( VM' (..)
   ,runVM
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

data VM' = VM' { stack :: Stack
             , fp    :: FP
             , pc    :: PC
             , instr :: Instruction
             } deriving (Read)

instance Show VM' where
  show = showVm

showVm :: VM' -> String
showVm VM' {stack = s, fp = fp, pc = pc, instr = instr} =
    "stack: " ++ show s  ++ "\n" ++
    "fp: "    ++ show fp ++ "\n" ++
    "pc: "    ++ show pc ++ "\n" ++
    "instr: " ++ show instr

initVM :: VM'
initVM = VM' {stack=initStack, fp=0, pc=0, instr=None}

type VM a = StateT VM' IO a

runVM :: [Instruction] -> IO ()
runVM is = evalStateT (mapM_ runInstr is) initVM

runInstr ::Instruction -> VM ()
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
  Jump        -> undefined
  Jumpz       -> undefined
  Load        -> undefined
  Store       -> undefined
  Slide       -> undefined
  Loadsp      -> undefined
  Loadfp      -> undefined
  Storefp     -> undefined
  Loadr       -> undefined
  Storer      -> undefined
  Halt        -> do liftIO exitSuccess
  where f i = do            -- printing intermediate state
          vm <- get         -- and modifying it
          modify i
          liftIO $ print vm
        printint = do       -- special case
          (VM' xs _ _ _) <- get
          liftIO $ print (xs!!(length xs -1))

-- VM instructions
loadc :: Int -> VM' -> VM'
loadc x (VM' s fp pc i) = VM' {stack=x:s
                               , fp=fp
                               , pc=incC pc
                               , instr=Loadc x
                              }

dup :: VM' -> VM'
dup (VM' (x:xs) fp pc i) = VM' {stack=x:x:xs
                               ,fp=fp
                               ,pc=incC pc
                               ,instr=Dup
                               }

pop :: VM' -> VM'
pop (VM' (x:xs) fp pc i) = VM' {stack=xs
                               ,fp=fp
                               ,pc=incC pc
                               ,instr=Pop
                               }

-- Arithmetics
type BinOp = (Int -> Int -> Int)

appBinOp :: Instruction -> BinOp -> VM' -> VM'
appBinOp instr op (VM' (x:y:xs) fp pc i) = VM' {stack=op y x:xs
                                      ,fp=fp
                                      ,pc=incC pc
                                      ,instr=instr
                                      }

type LogicalOp = (Int -> Int -> Bool)

appLogOp :: Instruction -> LogicalOp -> VM' -> VM'
appLogOp instr op (VM' (x:y:xs) fp pc i) = VM' {stack=g (op y x):xs
                                               ,fp=fp
                                               ,pc=incC pc
                                               ,instr=instr
                                               }
  where g c = fromEnum c

notOp :: VM' -> VM'
notOp (VM' (x:xs) fp pc i) = VM' {stack=g x:xs
                                 ,fp=fp
                                 ,pc=incC pc
                                 ,instr=Not
                                 }
  where g x = if x == 0 then 1 else 0
