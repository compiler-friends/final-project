-- This will take an AST expression output by the parser, evaluate it, and
-- return a list of Strings (produced by print statements in the program).

module ASTInterpreter where
  import Prelude hiding (lookup)
  import Data.Map(Map, lookup, insert, empty, fromList)  -- for State
