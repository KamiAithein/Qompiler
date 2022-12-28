module Quack.Common where

import qualified Data.HashMap.Strict as H


type Label = String
type Param = String
data LCExp  = LCLabel !Label
            | LCLam !Param !(Maybe LCExp)
            | LCApp !LCExp !LCExp
            deriving (Show, Eq)

type Definition = (Label, LCExp)
data LCProgram = LCProgram 
    { lcpEnv :: ![Definition]
    , lcpExp :: !LCExp
    }
    deriving (Show)

replaceInWith :: Param -> LCExp -> LCExp -> LCExp
replaceInWith param body@(LCLabel label) applied 
    | label == param = applied
    | otherwise      = body
replaceInWith param (LCApp exp exp') applied = 
    let replacer = (\body -> replaceInWith param body applied)
    in LCApp (replacer exp) (replacer exp')
replaceInWith param body applied = body