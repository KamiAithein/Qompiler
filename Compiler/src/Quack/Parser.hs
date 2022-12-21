module Quack.Parser where


import Quack.Common
import Control.Exception

data ParseError = InvalidSyntax !String
                deriving (Show)
instance Exception ParseError

parser  :: [Lexeme] -> Either ParseError AST
parser lxs = 
    case parser' lxs of
        (_, Right (Just ast)) -> Right ast
        (_, Right Nothing)    -> throw $ InvalidSyntax "Empty program!"
        (_, Left err)         -> throw err


parser' :: [Lexeme] -> ([Lexeme], Either ParseError (Maybe AST))
parser' [] = ([], Right Nothing)

parser' (LABEL s : rest) = 
    let label = ASTLabel s
    in case parser' rest of
            (rest', Right (Just exp)) -> (rest', Right $ Just $ ASTApp label exp)
            (rest', Right Nothing)  -> (rest', Right $ Just label)
            (_, Left err)           -> throw err

parser' (PARAMSTART : (LABEL (param:_)) : PARAMEND : body) = 
    case parser' body of
        (rest, Right bodyParse) -> 
                let func = ASTFunction param bodyParse
                in case parser' rest of
                        (rest', Right (Just exp)) -> (rest', Right $ Just $ ASTApp func exp)
                        (rest', Right Nothing)  -> (rest', Right $ Just func)
                        (_, Left err)           -> throw err
        (_, Left err)           -> throw err 

parser' (EXPSTART : exp) = 
    case parser' exp of
        (EXPEND : rest, Right (Just expParse)) -> 
            case parser' rest of
                    (rest, Right (Just exp')) -> (rest, Right $ Just $ ASTApp expParse exp')
                    (rest, Right Nothing)     -> (rest, Right $ Just expParse)
                    (_, Left err)             -> throw err
        (EXPEND : rest, Right Nothing) -> parser' rest -- ignore
        (_, Right _)  -> throw $ InvalidSyntax "Unmwatched parenthesis!"
        (_, Left err) -> throw err
parser' (EXPEND : rest) = (EXPEND : rest, Right Nothing)
parser' toks = throw $ InvalidSyntax $ "Unknown parse: " ++ show (take 3 toks) 