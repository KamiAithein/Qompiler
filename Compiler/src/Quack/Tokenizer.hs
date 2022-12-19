module Quack.Tokenizer where

import Quack.Common

tokenize :: SourceCode -> [Token]
tokenize = words