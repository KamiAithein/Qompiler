{-# LANGUAGE NamedFieldPuns #-}
module Quack.Evaluate where

import Quack.Common

import qualified Data.HashMap.Strict as H
import Data.List
import Control.Monad.State.Lazy

fixpointFrom :: (Eq a) => (a -> a) -> a -> a
fixpointFrom f a = 
    let a' = f a
    in  if a == a' 
        then a
        else fixpointFrom f a'

inlineVars :: LCExp -> [Definition] -> LCExp
inlineVars exp lcpEnv = 
    let env = H.fromList lcpEnv
    in inlineVars' [] env exp 

inlineVars' :: [Label] -> H.HashMap Label LCExp -> LCExp -> LCExp
inlineVars' labels env = inlineVars'' labels env
    where   inliner inlineLabels body = inlineVars'' inlineLabels env body 
            inlineVars'' :: [Label] -> H.HashMap Label LCExp -> LCExp -> LCExp

            inlineVars'' labels env exp@(LCLabel l) 
                | l `elem` labels = exp
                | otherwise         = H.lookupDefault exp l env

            inlineVars'' labels env exp@(LCApp l r) = 
                LCApp (inliner labels l) (inliner labels r)

            inlineVars'' labels env exp@(LCLam param (Just body))  =
                let labels' = (param:labels)
                in LCLam param $ Just $ inliner labels' body 

-- do something similar here as inlining: keep track of [Label] and (freshVar :: Int) and if lambda param in [Label] param=freshVar freshVar ++
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