module Quack.Model where

import Quack.Common
import Quack.Evaluate
import Quack.Parser
import Quack.Lexxer
import Quack.Tokenizer

import Data.Either
import qualified Data.HashMap.Strict as H

fullPipe :: String -> ParseTree
fullPipe = simplify . parsePipe

parsePipe =  fromRight (parseTreeFrom H.empty (ASTLabel "fail parse")) . parser . lexxer . tokenize