module Quack.Common where

import qualified Data.HashMap.Strict as H

type SourceCode = String
type Token = String

data Lexeme = EXPSTART
            | EXPEND
            | PARAMSTART
            | PARAMEND
            | LABEL String
            deriving (Show)