{-# LANGUAGE NamedFieldPuns #-}
module Quack.Evaluate where

import Quack.Common

import qualified Data.HashMap.Strict as H
import Data.List
import Control.Monad.State.Lazy

import Debug.Trace

type Bind = Param
data LCProgramEval = LCProgramEval
    { lcpeProgram   :: LCProgram 
    , lcpeFresh     :: Integer
    , lcpeBinds     :: [Bind]
    }

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

-- TODO change LCExp -> LCExp to LCProgram -> LCProgram
evaluate :: LCExp -> LCExp
evaluate exp = evalState (evaluate' exp) ([], [], 0) 

evaluate' :: LCExp -> State ([Label], [Label], Integer) LCExp
evaluate' (LCApp l r)  = trace "1" $ apply l r

evaluate' (LCLam p (Just b)) = trace "2" $ do
    (bindsl, bindsr, fresh) <- get
    put (p:bindsl, bindsr, fresh)
    b' <- evaluate' b 

    return $ LCLam p $ Just $ b'

evaluate' evaluated = trace "3" $ return evaluated

apply :: LCExp -> LCExp -> State ([Label], [Label], Integer) LCExp

apply (LCLam param (Just body)) applied = trace "4" $ do
    subInWithGiven param body applied 

apply l r = trace "5" $ return $ LCApp l r

subInWithGiven :: Param -> LCExp -> LCExp -> State ([Label], [Label], Integer) LCExp
subInWithGiven sub inn@(LCLabel l) with = trace "6" $do
    if l == sub
    then alphaRename with
    else return $ inn

subInWithGiven sub inn@(LCLam param (Just body)) with = trace "7" $do
    (bindsl, bindsr, fresh) <- get
    put (param:bindsl, bindsr, fresh)

    body' <- subInWithGiven sub body with 

    (bindsl', bindsr', fresh') <- get
    put (bindsl, bindsr, fresh')

    return $ LCLam param $ Just $ body'

subInWithGiven sub inn@(LCApp l r) with = trace "8" $ do
    (bindsl, bindsr, fresh) <- get

    l' <- subInWithGiven sub l with

    (_, _, fresh') <- get
    put (bindsl, bindsr, fresh')

    r' <- subInWithGiven sub r with

    (_, _, fresh'') <- get
    put (bindsl, bindsr, fresh'')


    return $ LCApp l' r'

alphaRename :: LCExp -> State ([Label], [Label], Integer) LCExp
alphaRename rename@(LCLabel l) = trace "9" $ do
    (bindsl, bindsr, fresh) <- get
    if (l `elem` bindsl) && (not $ l `elem` bindsr) 
        then do
            put (bindsl, bindsr, fresh + 1)
            return $ LCLabel $ show fresh
        else return rename

alphaRename (LCLam param (Just body)) = trace "10" $ do
    (bindsl, bindsr, fresh) <- get
    let bindsr' = (param:bindsr)
    put (param:bindsl, bindsr, fresh)

    body' <- alphaRename body

    (_, _, fresh') <- get
    put (bindsl, bindsr, fresh')

    return $ LCLam param $ Just $ body'

alphaRename (LCApp l r) = trace "11" $ do
    (bindsl, bindsr, fresh) <- get
    l' <- alphaRename l

    (_, _, fresh') <- get
    put (bindsl, bindsr, fresh')

    r' <- alphaRename r

    (_, _, fresh'') <- get
    put (bindsl, bindsr, fresh'')

    return $ LCApp l' r'