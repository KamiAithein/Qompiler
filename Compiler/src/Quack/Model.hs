module Quack.Model where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Exception
import Control.Monad

data CompilerError = InvalidToken
                   | InvalidSyntax
                   deriving (Show)
instance Exception CompilerError

data Token = STARTEXP   -- (
           | ENDEXP     -- )
           | STARTINDEX -- [
           | ENDINDEX   -- ]
           | WHITESPACE -- ' '
           | LABEL !String
           | LITERAL !String -- just deal with strings for now
           deriving (Show)

data Function = Function deriving Show -- TODO

type AST = [Token]
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
transferCompilerContext context value =
    do
        value' <- value
        context{value=value'}

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
            case lexxer $ freshCompilerBox $ reverse src of
                    Left err  -> throw err
                    Right res -> res

        parseResult =
            case parser lexResult of
                    Left err     -> throw err
                    Right res    -> res
    in Right $ show parseResult

lexxer :: CompilerBox String -> Either CompilerError (CompilerBox [Token])
lexxer boxedSrc =
    (\boxedSrc' -> do
            src <- boxedSrc'
            let src' = sanitize src
            let tokens = tokenizer src'
            case tokens of
                Left err -> throw err
                Right toks -> transferCompilerContext boxedSrc' $ freshCompilerBox toks

            ) <$> Right boxedSrc




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


tokenizer ::  [Char] -> Either CompilerError [Token]
tokenizer = tokenizer' (Right [])

tokenizer' :: Either CompilerError [Token] -> [Char] -> Either CompilerError [Token]
tokenizer'  err@(Left _) _ = err
tokenizer' toks [] = toks
 
tokenizer' (Right toks) src@('(':src') = tokenizer' (Right (STARTEXP:toks)) src'
tokenizer' (Right toks) src@(')':src') = tokenizer' (Right (ENDEXP:toks)) src'
tokenizer' (Right toks) src@('[':src') = tokenizer' (Right (STARTINDEX:toks)) src'
tokenizer' (Right toks) src@(']':src') = tokenizer' (Right (ENDINDEX:toks)) src'
tokenizer' (Right toks) src@(' ':src') = tokenizer' (Right (WHITESPACE:toks)) src'
tokenizer' (Right toks) src@(t:src')   = tokenizer' (Right (LABEL [t]:toks)) src'

tokenizer' (Right toks) unk = Left InvalidToken

parser :: CompilerBox [Token] -> Either CompilerError (CompilerBox AST)
parser = Right
