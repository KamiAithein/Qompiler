module Quack.Common where

import qualified Data.HashMap.Strict as H

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
            deriving (Show, Eq)

type Param = Char
data AST    = ASTLabel      !String
            | ASTApp        AST AST
            | ASTFunction   !Param (Maybe AST)
            deriving (Show)

data ParseTree = ParseTree
    { pAst     :: !AST
    , pContext :: !Context
    }
    deriving (Show)
    
type Pattern = String

type Context = H.HashMap Pattern AST
