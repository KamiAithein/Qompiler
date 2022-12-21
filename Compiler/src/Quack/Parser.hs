module Quack.Parser where

import Quack.Common

parser  :: [Lexeme] -> Maybe AST
parser lxs = 
    let (rest, ast) = parser' lxs
    in
        

parser' (EXPSTART:rest)  = 
    case parser' rest of
        (rest', Just exp) -> case parser' rest' of
                                (rest'', Just app) -> (rest'', Just $ ASTApp exp app)
                                (rest'', Nothing)  -> (rest'', Just exp)
        (rest', Nothing) ->  (rest', Nothing)

parser' (EXPEND:rest)    = (rest, Nothing)
parser' (PARAMSTART : LABEL (l:_) : PARAMEND : rest) = 
    let parse@(rest', res) = parser' rest
        func = 
            case parse of
                (rest', Just body) -> ASTFunction l (Just body)
                (rest', Nothing)   -> ASTFunction l (Nothing)
    
    in 
            case parser' rest' of
                (rest'', Just astRest) -> (rest'', Just $ ASTApp func astRest)
                ([], Nothing) -> ([], Just func)
                (rest'', Nothing) -> case parser' rest'' of
                                        (rest''', Just next) -> (rest''', Just $ ASTApp func next)
                                        (rest''', Nothing) -> (rest''', Just func)
parser' (LABEL l : rest) = 
    let appFunc = ASTApp (ASTLabel l)
    in
            case parser' rest of
                (rest', Just astRest) -> (rest', Just $ appFunc astRest)
                (rest', Nothing) -> (rest', Just $ ASTLabel l)
parser' _ = undefined
