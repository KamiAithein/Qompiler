module Quack.Lexxer where

import Control.Monad.State.Lazy
import qualified Data.HashMap.Strict as H
import Quack.Common

type LexemeMap = H.HashMap Token Lexeme

lexMap :: LexemeMap
lexMap = H.fromList [ ("(", EXPSTART)
                    , (")", EXPEND)
                    , ("\\", PARAMSTART)
                    , (".", PARAMEND)
                    , ("[", SCHEMPATSTART)
                    , ("]", SCHEMPATEND)
                    , (">", SCHEMDEFSTART)
                    , (";", SCHEMDEFEND)
                    ]


lexxer  :: [Token] -> [Lexeme]
lexxer = fmap (\k -> H.findWithDefault (LABEL k) k lexMap)
    



