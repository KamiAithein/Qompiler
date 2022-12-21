module Quack.Common where

type SourceCode = String
type Token = String

data Lexeme = EXPSTART
            | EXPEND
            | PARAMSTART
            | PARAMEND
            | LABEL !String
            deriving (Show)

type Param = Char
data AST    = ASTLabel      !String
            | ASTApp        AST AST
            | ASTFunction   !Param (Maybe AST)
            deriving (Show)