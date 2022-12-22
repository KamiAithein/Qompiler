module Quack.Common where

type SourceCode = String
type Token = String

data Lexeme = EXPSTART
            | EXPEND
            | PARAMSTART
            | PARAMEND
            | SCHEMPATSTART
            | SCHEMPATEND
            | SCHEMDEFSTART
            | SCHEMDEFEND
            | LABEL !String
            deriving (Show)

type Param = Char
data AST    = ASTLabel      !String
            | ASTApp        AST AST
            | ASTFunction   !Param (Maybe AST)
            deriving (Show)

data ParseTree = ParseTree
    { ast :: !AST
    }
    deriving (Show)
    
type Pattern = String
data Scheme = Scheme
    { sPattern      :: !Pattern
    , sDefinition   :: !AST 
    }