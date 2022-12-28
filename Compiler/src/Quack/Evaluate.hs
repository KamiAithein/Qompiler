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

instance Functor LCExp where
    fmap f (LCApp l r) = f $ LCApp (f <$> l) (f <$> r)
    fmap f (LCLam param (Just body)) = f $ LCLam param (Just f <$> body) 
    fmap f label@(LCLabel _) = f label 

simplify :: LCExp -> LCExp
simplify exp = fixpointFrom simplify' exp

simplify' (LCApp (LCLam param (Just body)) applied) = 
    replaceInWith param body applied
simplify' (LCApp exp exp') = LCApp (simplify exp) (simplify exp')

simplify' (LCLam param (Just body)) = LCLam param $ Just $ simplify body


-- anothing not defined is simplified
simplify' simplified = simplified