{-# LANGUAGE NamedFieldPuns #-}

module Quack.Model where

import Quack.Common
import Quack.Parsec
import Quack.Evaluate

import Text.ParserCombinators.Parsec

parseOut str = 
    let LCProgram{lcpExp} = parseProgramOut str
    in lcpExp

parseProgramOut str = 
    let Right lcp = parse lcProgram "fail?" str
    in lcp