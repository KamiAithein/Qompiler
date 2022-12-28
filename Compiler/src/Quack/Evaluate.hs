{-# LANGUAGE NamedFieldPuns #-}

module Quack.Evaluate where

import Quack.Common

import Data.Function

fixpointFrom :: (Eq a) => (a -> a) -> a -> a
fixpointFrom f a = 
    let a' = f a
    in if a == a' 
        then a
        else fixpointFrom f a'

spanMap :: (LCExp -> LCExp) -> LCExp -> LCExp
spanMap f (LCApp l r) = f $ LCApp (f `spanMap` l) (f `spanMap` r)
spanMap f (LCLam param (Just body)) = f $ LCLam param (Just $ f `spanMap` body) 
spanMap f label@(LCLabel _) = f label 

evaluate :: LCExp -> LCExp
evaluate exp = fixpointFrom evaluate' exp

evaluate' (LCApp (LCLam param (Just body)) applied) = 
    replaceInWith param body applied
evaluate' (LCApp exp exp') = LCApp (evaluate exp) (evaluate exp')

evaluate' (LCLam param (Just body)) = LCLam param $ Just $ evaluate body


-- anothing not defined is simplified
evaluate' simplified = simplified