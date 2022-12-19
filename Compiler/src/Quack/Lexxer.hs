module Quack.Lexxer where

import Control.Monad.State.Lazy
import qualified Data.HashMap.Strict as H
import Quack.Common


data LexxerStateRep = LexxerStateRep
    { lxrLexemes :: [Lexeme]
    , lxrTokens  :: [Token]
    , lxrLexMap  :: LexemeMap
    }
    deriving (Show)

type LexxerBox = State LexxerStateRep

type LexemeMap = H.HashMap (Token) (Lexeme)

lexMap :: LexemeMap
lexMap = H.fromList [ ("(", EXPSTART)
                    , (")", EXPEND)
                    , ("\\", PARAMSTART)
                    , (",", PARAMEND)
                    ]

defaultLexxerRep :: LexxerStateRep
defaultLexxerRep = LexxerStateRep
    { lxrLexemes=[]
    , lxrTokens=[]
    , lxrLexMap=lexMap
    } 


lexxer  :: [Token] -> [Lexeme]
lexxer toks = do
    let startState = defaultLexxerRep
    let startValue = lLex toks
    let (lxs, _)   =  runState startValue startState
    lxs
    

lLex :: [Token] -> State LexxerStateRep [Lexeme]
lLex [] = do
    lxr@LexxerStateRep{lxrLexemes=lxrLexemes} <- get
    return lxrLexemes
lLex toks = do
    lxr@LexxerStateRep{lxrLexemes=lxrLexemes, lxrTokens=lxrTokens, lxrLexMap=lxrLexMap}
    



