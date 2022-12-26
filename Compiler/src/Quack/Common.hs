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
            | ASTApp        !AST !AST
            | ASTFunction   !Param (Maybe AST)
            | ASTIdentity
            deriving (Show)

data ParseTree = ParseTree
    { pAst     :: !AST
    , pContext :: !Context
    }
    deriving (Show)
    
parseTreeFrom :: Context -> AST -> ParseTree
parseTreeFrom ctx ast  = ParseTree{pAst=ast, pContext=ctx}

type Pattern = String

type Context = H.HashMap Pattern AST
