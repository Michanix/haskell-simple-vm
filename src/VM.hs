module VM
  ( VM' (..)
  ) where

import Instruction

import Control.Monad.State
import Control.Monad.IO.Class (liftIO)

type FP = Int
type PC = Int
type Stack = [Int]

initStack :: Stack
initStack = []

-- increment fp and pc
incC :: Int -> Int
incC = (+1)

data VM' = VM' { stack :: Stack
             , fp    :: FP
             , pc    :: PC
             , instr :: Instruction
             } deriving (Read)

type VM a = StateT VM' IO a

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

runVM :: [Instruction] -> IO VM'
runVM is = execStateT (mapM_ runInstr is) initVM


runInstr ::Instruction -> VM ()
runInstr i = case i of
  (Loadc val) -> f (loadc val)
  Dup         -> f dup
  Pop         -> f pop
  Add         -> f (appBinOp i (+))
  Sub         -> f (appBinOp i (-))
  Div         -> f (appBinOp i (div))
  Mul         -> f (appBinOp i (*))
  Eq          -> f (appLogOp i (==))
  Leq         -> f (appLogOp i (<=))
  Not         -> f notOp
  where f i = do
          vm <- get
        --  maybe (return()) (\x -> modify (push' x)) (Just val)
          modify i
          liftIO $ print vm -- printing intermediate state

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

printint :: VM' -> VM'
printint (VM' (x:xs) fp pc i) = VM' {stack=xs
                                    ,fp=fp
                                    ,pc=incC pc
                                    ,instr=PrintInt}
