module Quack.Model where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Exception
import Control.Monad
import Control.Monad.Fix

import Data.List ( isPrefixOf, stripPrefix )
import Data.Either ( fromRight )
import qualified Data.Maybe

data CompilerError = InvalidToken
                   | InvalidSyntax
                   deriving (Show)
instance Exception CompilerError

data Token = STARTEXP   -- (
           | ENDEXP     -- )
           | STARTINDEX -- [
           | ENDINDEX   -- ]
           | ENDARG
           | WHITESPACE -- ' '
           | LABEL !String
           | LITERAL !String -- just deal with strings for now
           deriving (Show)

type Lexeme = Token

data Function = Function
    { param :: !String
    , body :: !AST
    }
    deriving (Show)

data AST = ASTApp !Function ![AST]
         | ASTLabel !String
         | ASTExpression !AST
         | ASTLiteral !String
         | ASTFunction !String !AST
         deriving (Show)


data CompilerBox a = CompilerBox
    { value  :: !a
    , funcs  :: ![(String, Function)]
    , labels :: ![(String, String)] -- just dealing with strings for now... 
    }

instance (Show a) => Show (CompilerBox a) where
    show CompilerBox{value=v} = show v

freshCompilerBox :: a -> CompilerBox a
freshCompilerBox val = CompilerBox{value=val,funcs=[],labels=[]}

transferCompilerContext :: CompilerBox context -> CompilerBox value -> CompilerBox value
transferCompilerContext context value@CompilerBox{value=val} = context{value=val}

unCompilerBox :: CompilerBox value -> value
unCompilerBox CompilerBox{value=v} = v


instance Functor CompilerBox where
    fmap f old@CompilerBox{value=v} = old{value=f v}

instance Applicative CompilerBox where
    pure = freshCompilerBox
    CompilerBox{value=f} <*> old@CompilerBox{value=a} = f <$> old -- possibly merge other parts of context

instance Monad CompilerBox where
    return = pure
    old@CompilerBox{value=val} >>= f =
        let CompilerBox{value=val'} = f val
        in old{value=val'}



compiler :: String -> Either CompilerError String
compiler src =
    let lexResult =
             case lexxer $ freshCompilerBox src of
                Left err  -> throw err
                Right res -> res

        parseResult =
            case parser lexResult of
                    Left err     -> throw err
                    Right res    -> res
    in Right $ show parseResult

lexxer :: CompilerBox String -> Either CompilerError (CompilerBox [Token])
lexxer boxedSrc = 
    Right $ (\src ->
        let src' = sanitize src
            cBox = case tokenize src' of
                Left err -> throw err
                Right toks -> transferCompilerContext boxedSrc $ freshCompilerBox toks
            tokens = unCompilerBox cBox

            cBox' = case tokenize'labelfix tokens of
                Left err   -> throw err
                Right toks -> transferCompilerContext boxedSrc toks

            tokens' = unCompilerBox cBox'


        in tokens'
    ) <$> boxedSrc

combiner :: [Token] -> Token -> [Token]
combiner [] tok = [tok]
combiner orig@(WHITESPACE:rest) WHITESPACE = orig
combiner orig@(LABEL a : rest) (LABEL b) = LABEL (b++a) : rest --backwards for rev later
combiner orig@(LABEL _:rest) WHITESPACE =
    let combined = combiner' orig
    in  (WHITESPACE:combined)
combiner orig new = new:orig

combiner' :: [Token] -> [Token]
combiner' [] = []
combiner' orig@(LABEL a:LABEL b:rest) = combiner' $ LABEL (a ++ b) : combiner' rest
combiner' ok = ok

tokenize'labelfix :: [Token] -> Either CompilerError (CompilerBox [Token])
tokenize'labelfix boxedToks = Right $ freshCompilerBox $ foldl combiner [] boxedToks

tokenize :: String -> Either CompilerError [Token]
tokenize src = tokenize'all (src, Right[])

tokenize'all :: (String, Either CompilerError [Token]) -> Either CompilerError [Token]
tokenize'all       ([], result) = result
tokenize'all state@(src, toks) =
    let state'@(src', toks'child) = tokenize' state in tokenize'all state'

tokenize'inner :: (String, Token) -> (String, Either CompilerError [Token]) -> (String, Either CompilerError [Token])
tokenize'inner _ ret@(src, error@(Left _)) = ret
tokenize'inner _ ([], _) = throw InvalidSyntax

tokenize'inner search@(toFind, toSet)  state0@(src, Right toks) =
    if toFind `isPrefixOf` src
        then
            let src'stripped'm = stripPrefix toFind src
                src'stripped = Data.Maybe.fromMaybe [] src'stripped'm
            in (src'stripped, Right $ toSet:toks)

        else
            let (src', result) = tokenize' state0
            in case result of
                err@(Left _) -> (src', err)
                Right toks' -> tokenize'inner search (src', Right toks')

tokenize' :: (String, Either CompilerError [Token]) -> (String, Either CompilerError [Token])
tokenize'   (src@('(':src'), Right toks) =
    let inner@(src'', result) = tokenize'inner (")", ENDEXP) (src', Right (STARTEXP:toks))
    in case result of
        err@(Left _)  -> (src'', err)
        Right toks' ->  (src'', Right toks')


tokenize'   (src@('[':src'), Right toks) = (src', Right (STARTINDEX:toks))
tokenize'   (src@(' ':src'), Right toks) = (src', Right (WHITESPACE:toks))
tokenize'   (src@('.':src'), Right toks) = (src', Right (ENDARG:toks))
tokenize'   (src@(t  :src'), Right toks) = (src', Right (LABEL [t]:toks))
tokenize'   state@([], toks)             = state
tokenize'   err@(src, Left _)          = err





sanitize :: String -> String
sanitize = filter (`Set.notMember` whitespace)

whitespace :: Set Char
whitespace = Set.fromList ['\n', '\t', '\r']

llize :: Integer -> String -> [String]
llize n [] = []
llize n src@(h:t) | n >= toInteger (length src) = src:llize n t
                  | otherwise =
    let ll   = take (fromIntegral n) src
    in ll:llize n t -- CHANGE TO TAIL RECURSION ASAP?? TODO!


parser :: CompilerBox [Token] -> Either CompilerError (CompilerBox AST)
parser cBox = Right $ parser' <$> cBox

parser' :: [Token] -> AST

