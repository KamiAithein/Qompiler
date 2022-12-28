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

type LangFuncLabel = String
type Param = Char

data LangFunc = LangFunc
    { lfLabel  :: LangFuncLabel
    , lfAction :: LangFuncType -> AST
    }

data LangFuncType = LFInt !Int

instance Show LangFunc where
    show LangFunc{lfLabel=lfLabel} = lfLabel

addFunc :: LangFunc
addFunc  = LangFunc
    { lfLabel = "+"
    , lfAction =
        \(LFInt x) -> ASTLangFunc $
            LangFunc
            {   lfLabel = "X+",
                lfAction = \(LFInt y) -> 
                    ASTLabel $ show $ x+y
            }
    }

data AST    = ASTLabel      !String
            | ASTApp        !AST !AST
            | ASTFunction   !Param (Maybe AST)
            | ASTLangFunc   !LangFunc
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
