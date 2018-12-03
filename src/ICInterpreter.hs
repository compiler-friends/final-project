module ICInterpreter where


-- Intermediate Code Interpreter for CS 320 Project
-- See Lecture 24 for details. Code examples
-- given at the bottom of this file. 


import Prelude

import qualified Data.Map as Map

import Debug.Trace


-- An IC program is a list of IC instructions
-- Op codes have been given a prime so that
-- you can use some of these again in your
-- AST for the source language

type IC_Program = [IC_Instruction]

data Op = Var' String | Val' Int

data IC_Instruction
        = Plus'  Op Op Op
        | Minus' Op Op Op
        | Times' Op Op Op
        | Div'   Op Op Op
        | Mod'   Op Op Op
        | Equal' Op Op Op
        | NotEq' Op Op Op
        | Lt'    Op Op Op
        | Gt'    Op Op Op
        | Le'    Op Op Op
        | Ge'    Op Op Op
        | And'   Op Op Op        
        | Or'    Op Op Op
        | Uminus' Op Op
        | Not'    Op Op
        | Assign' Op Op
        | Bzero'  Op Int
        | Jump'   Int
        | Call'   Int
        | Push'
        | Return'  Op
        | Print'  String Op
        | Halt'

instance Show Op where
  show (Var' x) = x
  show (Val' v) = show v

instance Show IC_Instruction where
  show (Plus' dest src1 src2) = (show dest) ++ " = " ++ (show src1) ++ " + " ++ (show src2)
  show (Minus' dest src1 src2) = (show dest) ++ " = " ++ (show src1) ++ " - " ++ (show src2)
  show (Times' dest src1 src2) = (show dest) ++ " = " ++ (show src1) ++ " * " ++ (show src2)
  show (Div' dest src1 src2) = (show dest) ++ " = " ++ (show src1) ++ " / " ++ (show src2)
  show (Mod' dest src1 src2) = (show dest) ++ " = " ++ (show src1) ++ " % " ++ (show src2)
  show (Uminus' dest src1) = (show dest) ++ " = - " ++ (show src1)
  show (Equal' dest src1 src2) = (show dest) ++ " = " ++ (show src1) ++ " == " ++ (show src2)
  show (NotEq' dest src1 src2) = (show dest) ++ " = " ++ (show src1) ++ " != " ++ (show src2)
  show (Lt' dest src1 src2) = (show dest) ++ " = " ++ (show src1) ++ " < " ++ (show src2)
  show (Gt' dest src1 src2) = (show dest) ++ " = " ++ (show src1) ++ " > " ++ (show src2)
  show (Le' dest src1 src2) = (show dest) ++ " = " ++ (show src1) ++ " <= " ++ (show src2)
  show (Ge' dest src1 src2) = (show dest) ++ " = " ++ (show src1) ++ " >= " ++ (show src2)
  show (And' dest src1 src2) = (show dest) ++ " = " ++ (show src1) ++ " && " ++ (show src2)
  show (Or' dest src1 src2) = (show dest) ++ " = " ++ (show src1) ++ " || " ++ (show src2)
  show (Not' dest src1) = (show dest) ++ " = ! " ++ (show src1)
  show (Assign' dest src) = (show dest) ++ " = " ++ (show src)
  show (Bzero' src addr) = "bzero " ++ (show src) ++ " " ++ (show addr)
  show (Jump' addr) = "jump " ++ (show addr)
  show (Push') = "push"
  show (Call' addr) = "call " ++ (show addr)
  show (Return' addr) = "return " ++ (show addr)
  show (Print' str src) = "print " ++ "\"" ++ str ++ "\" " ++ (show src)
  show (Halt') = "halt"


showICProgram :: IC_Program -> IO ()
showICProgram p = (showProgram' p 0)
                where
                   showProgram' [] _        = putStr "\n"
                   showProgram' (i:is) addr = do putStrLn $ (show addr) ++ ": " ++ (show i)
                                                 showProgram' is (addr+1)


-- The main driver for the IC interpreter is execute, which
-- takes a list of IC instructions and executes with a simple
-- run-time stack (a list of maps), as described in lecture.
-- It returns a list of Strings (maybe!) produced by the print
-- instructions. 


execute :: IC_Program -> Maybe [String]

execute p = execute_from p 0 [] [] False


-- This version does some simple tracing by printing out each instruction as
-- it is executed. 

executet :: IC_Program -> Maybe [String]

executet p = execute_from p 0 [] [] True


type Env = Map.Map String Int

type Memory = [Env]


execute_from :: IC_Program -> Int -> Memory -> [String] -> Bool -> Maybe [String]

execute_from [] _ _ _ _ = Just []

execute_from p pc rts out traceIt =
   let inst = if traceIt
                 then trace ((show pc) ++ ": " ++ (show (p!!pc))++"\t" ++ (show rts)) (p!!pc) 
                 else p!!pc
   in case inst of
      Halt'              -> Just out
      (Jump' addr)       -> execute_from p addr rts out traceIt
      (Bzero' src1 addr) -> do v <- getVal src1 rts
                               if v == 0
                                  then execute_from p addr rts out traceIt
                                  else execute_from p (pc+1) rts out traceIt
      (Push')            -> execute_from p (pc+1) (Map.empty:rts) out traceIt
      (Call' addr)       -> execute_from p addr (putVal (Var' "_ret_addr") (pc+1) rts) out traceIt
      (Return' src)      -> case rts of
                               [sf] -> execute_from p 2 [] out traceIt    -- return from main
                               (sf:rest) -> let (Just v)    = getVal src (sf:rest)
                                                rts'        = putVal (Var' "_ret_val") v rest
                                                (Just addr) = Map.lookup "_ret_addr" sf
                                            in execute_from p addr rts' out traceIt

      (Print' str src1)  -> do v <- getVal src1 rts
                               execute_from p (pc+1) rts (out ++ [str ++ (show v)]) traceIt
      inst               -> do newmem <- execute_assignment inst rts
                               execute_from p (pc+1) newmem out traceIt


-- return value if integer, otherwise look up variable stack starting from top
-- this is necessary in order to pass parameters, but it also gives the
-- language dynamic scoping. Solve this sometime!
getVal :: Op -> Memory -> Maybe Int
getVal (Val' v) _      = Just v
getVal (Var' x) []     = Nothing
getVal (Var' x) (sf:rest) =  case Map.lookup x sf of
                                Nothing -> getVal (Var' x) rest
                                jv       -> jv

-- put new value in top stack frame
putVal :: Op -> Int -> Memory -> Memory
putVal (Var' x) v []        = [Map.insert x v Map.empty]      -- should never be used
putVal (Var' x) v (sf:rest) = (Map.insert x v sf):rest

   

execute_assignment :: IC_Instruction -> Memory -> Maybe Memory

execute_assignment (Plus' dest src1 src2) rts     = do u <- getVal src1 rts
                                                       v <- getVal src2 rts
                                                       return $ putVal dest (u+v) rts
execute_assignment (Minus' dest src1 src2) rts    = do u <- getVal src1 rts
                                                       v <- getVal src2 rts
                                                       return $ putVal dest (u-v) rts
execute_assignment (Times'  dest src1 src2) rts   = do u <- getVal src1 rts
                                                       v <- getVal src2 rts
                                                       return $ putVal dest (u*v) rts
execute_assignment (Div' dest src1 src2) rts      = do u <- getVal src1 rts
                                                       v <- getVal src2 rts
                                                       return $ putVal dest (div u v) rts
execute_assignment (Mod'  dest src1 src2) rts     = do u <- getVal src1 rts
                                                       v <- getVal src2 rts
                                                       return $ putVal dest (mod u v) rts
execute_assignment (Uminus' dest src1) rts        = do u <- getVal src1 rts
                                                       return $ putVal dest (0-u) rts
execute_assignment (Assign' dest src1) rts        = do u <- getVal src1 rts
                                                       return $ putVal dest u rts
execute_assignment (Equal'  dest src1 src2) rts   = do u <- getVal src1 rts
                                                       v <- getVal src2 rts
                                                       return $ putVal dest (fromEnum(u==v)) rts
execute_assignment (NotEq' dest src1 src2) rts    = do u <- getVal src1 rts
                                                       v <- getVal src2 rts
                                                       return $ putVal dest (fromEnum(u/=v)) rts
execute_assignment (Lt'  dest src1 src2) rts      = do u <- getVal src1 rts
                                                       v <- getVal src2 rts
                                                       return $ putVal dest (fromEnum(u<v)) rts
execute_assignment (Gt' dest src1 src2) rts       = do u <- getVal src1 rts
                                                       v <- getVal src2 rts
                                                       return $ putVal dest (fromEnum(u>v)) rts
execute_assignment (Le'  dest src1 src2) rts      = do u <- getVal src1 rts
                                                       v <- getVal src2 rts
                                                       return $ putVal dest (fromEnum(u<=v)) rts
execute_assignment (Ge' dest src1 src2) rts       = do u <- getVal src1 rts
                                                       v <- getVal src2 rts
                                                       return $ putVal dest (fromEnum(u>=v)) rts
execute_assignment (And' dest src1 src2) rts      = do u <- getVal src1 rts
                                                       v <- getVal src2 rts
                                                       return $ putVal dest (fromEnum(u+v>1)) rts
execute_assignment (Or' dest src1 src2) rts       = do u <- getVal src1 rts
                                                       v <- getVal src2 rts
                                                       return $ putVal dest (fromEnum(u+v>0)) rts
execute_assignment (Not' dest src1) rts           = do u <- getVal src1 rts
                                                       return $ putVal dest (fromEnum(u==0)) rts
execute_assignment _ _ = Nothing


{-  Tests

    To see  pretty-printed versions of these, type

        showICProgram icTest1

    etc.

    To run the programs, type

        execute icTest1

    etc. or

        executet icTest1

    etc. 

-}

-- The numbering of tests corresponds to examples given in the project writeup.


icTest1 = [    (Push'), 
   (Call' 3), 
   (Halt'), 
   (Assign' (Var' "x") (Val' 6)), 
   (Assign' (Var' "y") (Val' 8)), 
   (Times' (Var' "_t1") (Var' "x") (Var' "y")), 
   (Div' (Var' "_t1") (Var' "_t1") (Val' 3)), 
   (Uminus' (Var' "_t2") (Var' "x")), 
   (Plus' (Var' "_t2") (Var' "_t1") (Var' "_t2")), 
   (Times' (Var' "_t1") (Val' 2) (Var' "y")), 
   (Plus' (Var' "_t1") (Var' "_t2") (Var' "_t1")), 
   (Assign' (Var' "z") (Var' "_t1")), 
   (Minus' (Var' "_t1") (Var' "x") (Val' 2)), 
   (Mod' (Var' "_t1") (Var' "x") (Var' "_t1")), 
   (Minus' (Var' "_t1") (Var' "z") (Var' "_t1")), 
   (Assign' (Var' "w") (Var' "_t1")), 
   (Print' "w = " (Var' "w")), 
   (Return' (Val' 0))  ]

icTest2 = [    (Push'), 
   (Call' 3), 
   (Halt'), 
   (Assign' (Var' "x") (Val' 4)), 
   (Assign' (Var' "y") (Val' 2)), 
   (Uminus' (Var' "_t1") (Val' 1)), 
   (Assign' (Var' "z") (Var' "_t1")), 
   (Gt' (Var' "_t1") (Var' "x") (Val' 2)), 
   (Bzero'  (Var' "_t1") 11), 
   (Jump' 10), 
   (Print' "x = " (Var' "x")), 
   (Lt' (Var' "_t2") (Var' "y") (Val' 2)), 
   (Bzero'  (Var' "_t2") 15), 
   (Jump' 14), 
   (Print' "y = " (Var' "y")), 
   (NotEq' (Var' "_t3") (Var' "z") (Val' 2)), 
   (Bzero'  (Var' "_t3") 20), 
   (Jump' 18), 
   (Print' "y = " (Var' "y")), 
   (Jump' 21), 
   (Print' "x = " (Var' "x")), 
   (Print' "z = " (Var' "z")), 
   (Le' (Var' "_t4") (Var' "z") (Var' "y")), 
   (Bzero'  (Var' "_t4") 34), 
   (Jump' 25), 
   (Print' "x = " (Var' "x")), 
   (Plus' (Var' "_t5") (Var' "x") (Var' "y")), 
   (Gt' (Var' "_t5") (Var' "_t5") (Var' "z")), 
   (Bzero'  (Var' "_t5") 32), 
   (Jump' 30), 
   (Print' "y = " (Var' "y")), 
   (Jump' 33), 
   (Print' "z = " (Var' "z")), 
   (Jump' 35), 
   (Print' "z = " (Var' "z")), 
   (Return' (Val' 0))  ]

icTest3 = [    (Push'), 
   (Call' 3), 
   (Halt'), 
   (Assign' (Var' "k") (Val' 1)), 
   (Assign' (Var' "sum") (Val' 0)), 
   (Le' (Var' "_t1") (Var' "k") (Val' 10)), 
   (Bzero'  (Var' "_t1") 13), 
   (Jump' 8), 
   (Plus' (Var' "_t2") (Var' "sum") (Var' "k")), 
   (Assign' (Var' "sum") (Var' "_t2")), 
   (Plus' (Var' "_t2") (Var' "k") (Val' 1)), 
   (Assign' (Var' "k") (Var' "_t2")), 
   (Jump' 5), 
   (Print' "sum = " (Var' "sum")), 
   (Return' (Val' 0))  ]

icTest4 = [    (Push'), 
   (Call' 3), 
   (Halt'), 
   (Assign' (Var' "n") (Val' 1)), 
   (Assign' (Var' "count") (Val' 0)), 
   (Le' (Var' "_t1") (Var' "n") (Val' 3)), 
   (Bzero'  (Var' "_t1") 24), 
   (Jump' 8), 
   (Assign' (Var' "m") (Val' 1)), 
   (Le' (Var' "_t2") (Var' "m") (Val' 4)), 
   (Bzero'  (Var' "_t2") 21), 
   (Jump' 12), 
   (Mod' (Var' "_t3") (Var' "m") (Var' "n")), 
   (Equal' (Var' "_t3") (Var' "_t3") (Val' 0)), 
   (Bzero'  (Var' "_t3") 18), 
   (Jump' 16), 
   (Plus' (Var' "_t4") (Var' "count") (Val' 1)), 
   (Assign' (Var' "count") (Var' "_t4")), 
   (Plus' (Var' "_t4") (Var' "m") (Val' 1)), 
   (Assign' (Var' "m") (Var' "_t4")), 
   (Jump' 9), 
   (Plus' (Var' "_t4") (Var' "n") (Val' 1)), 
   (Assign' (Var' "n") (Var' "_t4")), 
   (Jump' 5), 
   (Print' "count = " (Var' "count")), 
   (Return' (Val' 0))  ]

icTest5 = [    (Push'), 
   (Call' 3), 
   (Halt'), 
   (Assign' (Var' "count") (Val' 0)), 
   (Assign' (Var' "limit") (Val' 10)), 
   (Assign' (Var' "n") (Val' 2)), 
   (Le' (Var' "_t1") (Var' "count") (Var' "limit")), 
   (Bzero'  (Var' "_t1") 31), 
   (Jump' 9), 
   (Assign' (Var' "isPrime") (Val' 1)), 
   (Assign' (Var' "k") (Val' 2)), 
   (Lt' (Var' "_t2") (Var' "k") (Var' "n")), 
   (Bzero'  (Var' "_t2") 22), 
   (Jump' 14), 
   (Mod' (Var' "_t3") (Var' "n") (Var' "k")), 
   (Equal' (Var' "_t3") (Var' "_t3") (Val' 0)), 
   (Bzero'  (Var' "_t3") 19), 
   (Jump' 18), 
   (Assign' (Var' "isPrime") (Val' 0)), 
   (Plus' (Var' "_t4") (Var' "k") (Val' 1)), 
   (Assign' (Var' "k") (Var' "_t4")), 
   (Jump' 11), 
   (Equal' (Var' "_t4") (Var' "isPrime") (Val' 1)), 
   (Bzero'  (Var' "_t4") 28), 
   (Jump' 25), 
   (Print' "n = " (Var' "n")), 
   (Plus' (Var' "_t5") (Var' "count") (Val' 1)), 
   (Assign' (Var' "count") (Var' "_t5")), 
   (Plus' (Var' "_t5") (Var' "n") (Val' 1)), 
   (Assign' (Var' "n") (Var' "_t5")), 
   (Jump' 6), 
   (Return' (Val' 0))  ]

icTest6 = [    (Push'), 
   (Call' 3), 
   (Halt'), 
   (Assign' (Var' "x") (Val' 3)), 
   (Assign' (Var' "y") (Val' 5)), 
   (Assign' (Var' "z") (Val' 1)), 
   (Gt' (Var' "_t1") (Var' "x") (Val' 2)), 
   (Bzero'  (Var' "_t1") 13), 
   (Jump' 9), 
   (Lt' (Var' "_t2") (Var' "y") (Val' 5)), 
   (Bzero'  (Var' "_t2") 13), 
   (Jump' 12), 
   (Print' "x = " (Var' "x")), 
   (Gt' (Var' "_t3") (Var' "x") (Val' 2)), 
   (Bzero'  (Var' "_t3") 16), 
   (Jump' 19), 
   (Lt' (Var' "_t4") (Var' "y") (Val' 5)), 
   (Bzero'  (Var' "_t4") 20), 
   (Jump' 19), 
   (Print' "x = " (Var' "x")), 
   (Gt' (Var' "_t5") (Var' "x") (Val' 2)), 
   (Bzero'  (Var' "_t5") 26), 
   (Jump' 23), 
   (Lt' (Var' "_t6") (Var' "y") (Val' 5)), 
   (Bzero'  (Var' "_t6") 26), 
   (Jump' 29), 
   (Equal' (Var' "_t7") (Var' "z") (Val' 2)), 
   (Bzero'  (Var' "_t7") 29), 
   (Jump' 30), 
   (Print' "x = " (Var' "x")), 
   (NotEq' (Var' "_t8") (Var' "z") (Val' 2)), 
   (Bzero'  (Var' "_t8") 33), 
   (Jump' 39), 
   (Gt' (Var' "_t9") (Var' "x") (Val' 2)), 
   (Bzero'  (Var' "_t9") 40), 
   (Jump' 36), 
   (Lt' (Var' "_t10") (Var' "y") (Val' 5)), 
   (Bzero'  (Var' "_t10") 40), 
   (Jump' 39), 
   (Print' "x = " (Var' "x")), 
   (Return' (Val' 0))  ]

icTest7 = [    (Push'), 
   (Call' 8), 
   (Halt'), 
   (Plus' (Var' "_t1") (Var' "x") (Val' 1)), 
   (Assign' (Var' "n") (Var' "_t1")), 
   (Times' (Var' "_t1") (Var' "n") (Val' 2)), 
   (Assign' (Var' "n") (Var' "_t1")), 
   (Return' (Var' "n")), 
   (Assign' (Var' "y") (Val' 2)), 
   (Assign' (Var' "n") (Val' 3)), 
   (Plus' (Var' "_t1") (Var' "y") (Var' "n")), 
   (Push'), 
   (Assign' (Var' "x") (Var' "_t1")), 
   (Call' 3), 
   (Assign' (Var' "_t2") (Var' "_ret_val")), 
   (Assign' (Var' "z") (Var' "_t2")), 
   (Print' "z = " (Var' "z")), 
   (Return' (Val' 0))  ]

icTest8 = [  (Push'), 
   (Call' 5), 
   (Halt'), 
   (Plus' (Var' "_t1") (Var' "x") (Val' 1)), 
   (Return' (Var' "_t1")), 
   (Assign' (Var' "a") (Val' 1)), 
   (Push'), 
   (Assign' (Var' "x") (Var' "a")), 
   (Call' 3), 
   (Assign' (Var' "_t2") (Var' "_ret_val")), 
   (Plus' (Var' "_t3") (Var' "a") (Val' 1)), 
   (Push'), 
   (Assign' (Var' "x") (Var' "_t3")), 
   (Call' 3), 
   (Assign' (Var' "_t4") (Var' "_ret_val")), 
   (Times' (Var' "_t5") (Var' "a") (Val' 2)), 
   (Push'), 
   (Assign' (Var' "x") (Var' "_t5")), 
   (Call' 3), 
   (Assign' (Var' "_t6") (Var' "_ret_val")), 
   (Times' (Var' "_t6") (Var' "_t4") (Var' "_t6")), 
   (Plus' (Var' "_t6") (Var' "_t2") (Var' "_t6")), 
   (Assign' (Var' "z") (Var' "_t6")), 
   (Print' "z = " (Var' "z")), 
   (Return' (Val' 0))  ]

icTest9 = [    (Push'), 
   (Call' 5), 
   (Halt'), 
   (Plus' (Var' "_t1") (Var' "x") (Val' 1)), 
   (Return' (Var' "_t1")), 
   (Assign' (Var' "a") (Val' 5)), 
   (Push'), 
   (Assign' (Var' "x") (Var' "a")), 
   (Call' 3), 
   (Assign' (Var' "_t2") (Var' "_ret_val")), 
   (Push'), 
   (Assign' (Var' "x") (Var' "_t2")), 
   (Call' 3), 
   (Assign' (Var' "_t3") (Var' "_ret_val")), 
   (Push'), 
   (Assign' (Var' "x") (Var' "_t3")), 
   (Call' 3), 
   (Assign' (Var' "_t4") (Var' "_ret_val")), 
   (Assign' (Var' "z") (Var' "_t4")), 
   (Print' "z = " (Var' "z")), 
   (Return' (Val' 0))  ]

icTest10 =  [ (Push'), 
   (Call' 18), 
   (Halt'), 
   (Plus' (Var' "_t1") (Var' "x") (Val' 1)), 
   (Return' (Var' "_t1")), 
   (Times' (Var' "_t2") (Var' "x") (Val' 2)), 
   (Return' (Var' "_t2")), 
   (Push'), 
   (Assign' (Var' "x") (Var' "y")), 
   (Call' 3), 
   (Assign' (Var' "_t3") (Var' "_ret_val")), 
   (Assign' (Var' "z") (Var' "_t3")), 
   (Push'), 
   (Assign' (Var' "x") (Var' "z")), 
   (Call' 5), 
   (Assign' (Var' "_t3") (Var' "_ret_val")), 
   (Assign' (Var' "y") (Var' "_t3")), 
   (Return' (Var' "y")), 
   (Push'), 
   (Assign' (Var' "y") (Val' 10)), 
   (Call' 7), 
   (Assign' (Var' "_t3") (Var' "_ret_val")), 
   (Assign' (Var' "z") (Var' "_t3")), 
   (Print' "z = " (Var' "z")), 
   (Return' (Val' 0))  ]

icTest11 = [  (Push'), 
   (Call' 13), 
   (Halt'), 
   (Assign' (Var' "a") (Val' 2868)), 
   (NotEq' (Var' "_t1") (Var' "b") (Val' 0)),
   (Bzero' (Var' "_t1") 12), 
   (Jump' 7), 
   (Assign' (Var' "t") (Var' "b")), 
   (Mod' (Var' "_t2") (Var' "a") (Var' "b")), 
   (Assign' (Var' "b") (Var' "_t2")), 
   (Assign' (Var' "a") (Var' "t")), 
   (Jump' 4), 
   (Return' (Var' "a")), 
   (Assign' (Var' "m") (Val' 264)), 
   (Push'), 
   (Assign' (Var' "b") (Var' "m")), 
   (Call' 3), 
   (Assign' (Var' "_t2") (Var' "_ret_val")), 
   (Assign' (Var' "res") (Var' "_t2")), 
   (Print' "res = " (Var' "res")), 
   (Return' (Val' 0))  ]

icTest12 = [    (Push'), 
   (Call' 30), 
   (Halt'), 
   (Le' (Var' "_t1") (Var' "n") (Val' 2)), 
   (Bzero' (Var' "_t1" ) 8), 
   (Jump' 6), 
   (Return' (Val' 1)), 
   (Jump' 30), 
   (Minus' (Var' "_t2") (Var' "n") (Val' 1)), 
   (Push'), 
   (Assign' (Var' "n") (Var' "_t2")), 
   (Call' 3), 
   (Assign' (Var' "_t3") (Var' "_ret_val")), 
   (Minus' (Var' "_t3") (Var' "n") (Var' "_t3")), 
   (Push'), 
   (Assign' (Var' "n") (Var' "_t3")), 
   (Call' 3), 
   (Assign' (Var' "_t4") (Var' "_ret_val")), 
   (Minus' (Var' "_t5") (Var' "n") (Val' 2)), 
   (Push'), 
   (Assign' (Var' "n") (Var' "_t5")), 
   (Call' 3), 
   (Assign' (Var' "_t6") (Var' "_ret_val")), 
   (Minus' (Var' "_t6") (Var' "n") (Var' "_t6")), 
   (Push'), 
   (Assign' (Var' "n") (Var' "_t6")), 
   (Call' 3), 
   (Assign' (Var' "_t7") (Var' "_ret_val")), 
   (Plus' (Var' "_t7") (Var' "_t4") (Var' "_t7")), 
   (Return' (Var' "_t7")), 
   (Assign' (Var' "k") (Val' 1)), 
   (Lt' (Var' "_t4") (Var' "k") (Val' 20)), 
   (Bzero' (Var' "_t4" ) 43), 
   (Jump' 34), 
   (Push'), 
   (Assign' (Var' "n") (Var' "k")), 
   (Call' 3), 
   (Assign' (Var' "_t8") (Var' "_ret_val")), 
   (Assign' (Var' "q") (Var' "_t8")), 
   (Print' "q = " (Var' "q")), 
   (Plus' (Var' "_t8") (Var' "k") (Val' 1)), 
   (Assign' (Var' "k") (Var' "_t8")), 
   (Jump' 31),
   (Return' (Val' 0)) ]

