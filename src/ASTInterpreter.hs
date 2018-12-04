-- This will take an AST expression output by the parser, evaluate it, and
-- return a list of Strings (produced by print statements in the program).

module ASTInterpreter where
import Prelude hiding (lookup)
import Data.Map(Map, lookup, insert, empty, fromList)  -- for State


type Program =[Stmts]

data Stmts = Identifier Expr
            |Def String [String] Stmts
            |Assign Expr Expr
            |Block [Stmts]
            |While Expr [Stmts]
            |If Expr [Stmts]
            |IfElse Expr [Stmts] [Stmts]
            |Return Expr
            |Print Expr
            |Break
            |Continue  


data Expr = Mult Expr Expr
           |Div Expr Expr
           |Plus Expr Expr
           |Minus Expr Expr
           |ValInt Integer
           |ValBool Bool
           |And Expr Expr
           |Or Expr Expr
           |Not Expr
           |Var String
           |Equ Expr Expr 
           |NotEqu Expr Expr
           |Lessthan Expr Expr
           |GreaterThan Expr Expr
           |LessthanEqu Expr Expr
           |GreaterThanEqu Expr Expr


