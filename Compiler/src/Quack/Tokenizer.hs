module Quack.Tokenizer where

import Quack.Common

import Data.List

tokenize :: SourceCode -> [Token]
tokenize = words . intersperse ' '