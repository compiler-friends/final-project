-- This will take an AST expression and convert it into an Intermediate Code
-- program (a list of IC statements). This IC program can then be executed
-- by the IC interpreter provided to generate output from the print
-- statements in the program.

module CCompiler where
  import CParser
  import ASTInterpreter
