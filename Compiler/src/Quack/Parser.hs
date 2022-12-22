module Quack.Parser where


import Quack.Common
import Control.Exception
import qualified Data.Either as E
{-
S -> LS
S -> \A.S S
S -> (S) S
S -> epsilon
-}
data ParseError = InvalidSyntax !String
                deriving (Show)
instance Exception ParseError



-- for now no contexts
combineParseTrees :: ParseTree -> ParseTree -> ParseTree
combineParseTrees a b = a

parser  :: [Lexeme] -> Either ParseError ParseTree
parser lxs = 
    case parser' lxs of
        (_, Right (Just ast)) -> Right ast
        (_, Right Nothing)    -> throw $ InvalidSyntax "Empty program!"
        (_, Left err)         -> throw err

parser' :: [Lexeme] -> ([Lexeme], Either ParseError (Maybe ParseTree))
parser' [] = ([], Right Nothing)

parser' (LABEL s : rest) = 
    let label = ASTLabel s
    in case parser' rest of
            (rest', Right (Just pexp@ParseTree{ast=exp})) -> (rest', Right $ Just $ pexp{ast=ASTApp label exp})
            (rest', Right Nothing)  -> (rest', Right $ Just ParseTree{ast=label})
            (_, Left err)           -> throw err

--FIX THIS we just take first letter of labels
parser' (PARAMSTART : (LABEL (param:_)) : PARAMEND : body) = 
    let (rest, pBodyParse) = 
            case parser' body of
                (_, Left err)           -> throw err 
                (rest, Right pBodyParse) -> (rest, pBodyParse)

        funcExp@ParseTree{ast=func} = 
            case pBodyParse of
                Nothing -> 
                    ParseTree{ast=ASTFunction param Nothing}
                Just ctx@ParseTree{ast=body} -> 
                    ctx{ast=ASTFunction param $ Just body}
        
        in case parser' rest of
                (rest', Right (Just pExp@ParseTree{ast=exp})) -> 
                        -- this leads to defintions being global
                        let combinedContexts = combineParseTrees pExp funcExp
                        in (rest', Right $ Just $ combinedContexts{ast=ASTApp func exp})

                (rest', Right Nothing)                        -> 
                    (rest', Right $ Just funcExp)

                (_, Left err)                                 -> throw err

parser' (EXPSTART : exp) = 
    case parser' exp of
        (EXPEND : rest, Right (Just pExp@ParseTree{ast=pExpAST})) -> 
            case parser' rest of
                    (rest', Right (Just pExp'@ParseTree{ast=pExp'AST})) -> 
                        let combinedContexts = combineParseTrees pExp pExp'
                        in (rest', Right $ Just $ combinedContexts{ast=ASTApp pExpAST pExp'AST})
                    (rest, Right Nothing)     -> (rest, Right $ Just pExp)
                    (_, Left err)             -> throw err

        (EXPEND : rest, Right Nothing) -> parser' rest -- ignore
        (_, Right _)  -> throw $ InvalidSyntax "Unmwatched parenthesis!"
        (_, Left err) -> throw err
parser' (EXPEND : rest) = (EXPEND : rest, Right Nothing)

parser' toks = throw $ InvalidSyntax $ "Unknown parse: " ++ show (take 3 toks) 