{-# LANGUAGE NamedFieldPuns #-}

module Quack.Model where

import Quack.Common
import Quack.Parsec
import Quack.Evaluate

import Text.ParserCombinators.Parsec

parseOut str = 
    let Right LCProgram{lcpExp} = parse lcProgram "fail?" str
    in lcpExp