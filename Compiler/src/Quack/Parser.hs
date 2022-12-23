module Quack.Parser where


import Quack.Common
import Control.Exception
import Data.List
import qualified Data.Either as E
import qualified Data.HashMap.Strict as H
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
combineParseTrees take@ParseTree{pContext=ctx} ParseTree{pContext=ctx'} =
    take{pContext=ctx `H.union` ctx'}

parser  :: [Lexeme] -> Either ParseError ParseTree
parser lxs =
    case parser' lxs of
        (_, Right (Just tree)) -> Right tree
        (_, Right Nothing)    -> throw $ InvalidSyntax "Empty program!"
        (_, Left err)         -> throw err

parser' :: [Lexeme] -> ([Lexeme], Either ParseError (Maybe ParseTree))
parser' [] = ([], Right Nothing)
parser' all@(SCHEMDEFEND : rest) = (all, Right Nothing)

parser' (LABEL s : rest) =
    let label = ASTLabel s
    in case parser' rest of
            (rest', Right (Just pexp@ParseTree{pAst=exp})) -> (rest', Right $ Just $ pexp{pAst=ASTApp label exp})
            (rest', Right Nothing)  -> (rest', Right $ Just ParseTree{pAst=label, pContext=H.empty})
            (_, Left err)           -> throw err

--FIX THIS we just take first letter of labels
parser' (PARAMSTART : (LABEL (param:_)) : PARAMEND : body) =
    let (rest, pBodyParse) =
            case parser' body of
                (_, Left err)           -> throw err
                (rest, Right pBodyParse) -> (rest, pBodyParse)

        funcExp@ParseTree{pAst=func} =
            case pBodyParse of
                Nothing ->
                    ParseTree{pAst=ASTFunction param Nothing, pContext=H.empty}
                Just ctx@ParseTree{pAst=body} ->
                    ctx{pAst=ASTFunction param $ Just body}

        in case parser' rest of
                (rest', Right (Just pExp@ParseTree{pAst=exp})) ->
                        -- this leads to defintions being global
                        let combinedContexts = combineParseTrees pExp funcExp
                        in (rest', Right $ Just $ combinedContexts{pAst=ASTApp func exp})

                (rest', Right Nothing)                        ->
                    (rest', Right $ Just funcExp)

                (_, Left err)                                 -> throw err

parser' (EXPSTART : exp) =
    case parser' exp of
        (EXPEND : rest, Right (Just pExp@ParseTree{pAst=pExpAST})) ->
            case parser' rest of
                    (rest', Right (Just pExp'@ParseTree{pAst=pExp'AST})) ->
                        let combinedContexts = combineParseTrees pExp pExp'
                        in (rest', Right $ Just $ combinedContexts{pAst=ASTApp pExpAST pExp'AST})

                    (rest, Right Nothing)     -> (rest, Right $ Just pExp)
                    (_, Left err)             -> throw err

        (EXPEND : rest, Right Nothing) -> parser' rest -- ignore
        (_, Right _)  -> throw $ InvalidSyntax "Unmwatched parenthesis!"
        (_, Left err) -> throw err
parser' (EXPEND : rest) = (EXPEND : rest, Right Nothing)

parser' (SCHEMPATSTART : patStr) =
    let (SCHEMPATEND   : SCHEMDEFSTART : sDef, pattern) = parsePattern patStr
        (SCHEMDEFEND  : rest, mPDef) = parser' sDef
        (rest', next) =
            case parser' rest of
                    ([], Right (Just program)) -> ([], program)
                    (_, Right _) -> throw $ InvalidSyntax "tokens left at end of parse!"
                    (_, Left err) -> throw err
    in case mPDef of
            Left err            -> throw err
            Right (Just pDef@ParseTree{pAst=ast})   ->
                let pDefWScheme = pDef{pContext=H.fromList[(pattern, ast)]}
                in ([], Right $ Just $ combineParseTrees next pDefWScheme)
            Right Nothing       -> throw $ InvalidSyntax "Empty definition!"
    where   parsePattern :: [Lexeme] -> ([Lexeme], Pattern)
            parsePattern lxs =
                let (patternBits, rest) =  span (/= SCHEMPATEND) lxs
                in (rest, combinePatternBits patternBits)
                where   combinePatternBits  :: [Lexeme] -> Pattern
                        combinePatternBits bits = concatMap (\(LABEL l) -> l) bits

parser' toks = throw $ InvalidSyntax $ "Unknown parse: " ++ show (take 3 toks)