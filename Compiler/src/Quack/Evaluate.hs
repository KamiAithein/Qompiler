{-# LANGUAGE NamedFieldPuns #-}
module Quack.Evaluate where

import Quack.Common

import qualified Data.HashMap.Strict as H

fixpointFrom :: (Eq a) => (a -> a) -> a -> a
fixpointFrom f a = 
    let a' = f a
    in  if a == a' 
        then a
        else fixpointFrom f a'

spanMap :: (LCExp -> LCExp) -> LCExp -> LCExp
spanMap f (LCApp l r) = f $ LCApp (f `spanMap` l) (f `spanMap` r)
spanMap f (LCLam param (Just body)) = f $ LCLam param (Just $ f `spanMap` body) 
spanMap f label@(LCLabel _) = f label 


inlineVars :: LCExp -> [Definition] -> LCExp
inlineVars exp lcpEnv = 
    let env = H.fromList lcpEnv
    in spanMap (varMapper $ env) exp 
    where   varMapper :: H.HashMap Label LCExp -> LCExp -> LCExp
            varMapper env orig@(LCLabel l) = H.lookupDefault orig l env 
            varMapper env other = other

evaluate :: LCProgram -> LCExp
evaluate LCProgram{lcpExp=exp, lcpEnv} =
    let exp' = inlineVars exp lcpEnv 
    in fixEval exp'
    
fixEval :: LCExp -> LCExp
fixEval = fixpointFrom evaluate' 

evaluate' (LCApp (LCLam param (Just body)) applied) = 
    replaceInWith param body applied
evaluate' (LCApp exp exp') = LCApp (fixEval exp) (fixEval exp')

evaluate' (LCLam param (Just body)) = LCLam param $ Just $ (fixEval body)


-- anothing not defined is simplified
evaluate' simplified = simplified