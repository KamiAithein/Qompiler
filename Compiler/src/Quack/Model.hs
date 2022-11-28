module Quack.Model where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Exception
import Control.Monad

import Debug.Trace

data CompilerError = InvalidToken
                   | InvalidSyntax
                   deriving (Show)
instance Exception CompilerError

data Token = STARTEXP   -- (
           | ENDEXP     -- )
           | STARTINDEX -- [
           | ENDINDEX   -- ]
           | LABEL String
           | LITERAL String -- just deal with strings for now
           deriving (Show)

data Function = Function deriving Show -- TODO

type AST = [Token]
data CompilerBox a = CompilerBox
    { value  :: a
    , funcs  :: [(String, Function)]
    , labels :: [(String, String)] -- just dealing with strings for now... 
    }

instance (Show a) => Show (CompilerBox a) where
    show CompilerBox{value=v} = show v ++ "finally???"

freshCompilerBox :: a -> CompilerBox a
freshCompilerBox val = trace "im fresh!" $ CompilerBox{value=val,funcs=[],labels=[]}

transferCompilerContext :: (Show value) => CompilerBox context -> CompilerBox value -> CompilerBox value
transferCompilerContext context value = trace "im transfering!" $ 
    do
        let apple = traceShowId "salad"
        value' <- traceShowId $ value 
        context{value=value'}

instance Functor (CompilerBox) where
    fmap f old@(CompilerBox{value=v}) = old{value=(f v)}

instance Applicative (CompilerBox) where
    pure = freshCompilerBox
    CompilerBox{value=f} <*> old@CompilerBox{value=a} = old{value=(f a)} -- possibly merge other parts of context

instance Monad (CompilerBox) where
    return = pure
    old@CompilerBox{value=val} >>= f = do 
        val' <- f val
        old{value=val'} -- Deal with combining environments??

        

compiler :: String -> Either CompilerError String
compiler src = 
    let lexResult = trace "dsadas1 " $ traceShowId $
            case (lexxer $ freshCompilerBox src) of
                    Left err  -> throw err
                    Right res -> res

        parseResult = trace "dsads2 " $ traceShowId $
            case (parser lexResult) of
                    Left err     -> throw err
                    Right res    -> res
    in Right $ show parseResult

lexxer :: CompilerBox String -> Either CompilerError (CompilerBox [Token])
lexxer boxedSrc =
    trace "god fucking???" $ traceShowId $ liftM (\boxedSrc' -> do
            src <- boxedSrc' 
            let src' = sanitize src
            -- tokenBox <- trace "AAA" $ traceShowId $ transferCompilerContext boxedSrc $ freshCompilerBox []
            let tokens = trace "what the fuck?/?" $ traceShowId $ foldl tokenizer (Right []) $ llize 2 src'
            case tokens of
                Left err -> throw err
                Right toks -> trace "this makes no sense" $ transferCompilerContext boxedSrc' $ freshCompilerBox toks
            ) $ trace "this doesnt show up???" $ (Right $ trace "what the fuckkkkk" $ boxedSrc)
    

        

sanitize :: String -> String
sanitize = filter (\c -> (c `Set.notMember` whitespace))

whitespace = Set.fromList [' ', '\n', '\t', '\r']

llize :: Integer -> [Char] -> [[Char]]
llize n [] = []
llize n src@(h:t) | n >= (toInteger $ length src) = src:(llize n $ tail src)
            | otherwise =
    let ll   = take (fromIntegral n) src
    in ll:(llize n $ tail src) -- CHANGE TO TAIL RECURSION ASAP?? TODO!


    

tokenizer :: Either CompilerError [Token] -> [Char] -> Either CompilerError [Token]
tokenizer a b = trace ("aa" ++ show a ++ show b) $ tokenizer' a b

tokenizer' err@(Left _) _ = err
tokenizer' (Right toks) ('(':_) = Right (STARTEXP:toks)
tokenizer' (Right toks) (')':_) = Right (ENDEXP:toks)
tokenizer' (Right toks) (t:_)   = Right (LABEL [t]:toks)

tokenizer' (Right toks) unk = Left InvalidToken
        
parser :: CompilerBox [Token] -> Either CompilerError (CompilerBox AST)
parser box = Right box
