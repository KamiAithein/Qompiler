module Quack.Common where

import qualified Data.HashMap.Strict as H


type Label = String
type Param = String
data LCExp  = LCLabel !Label
            | LCLam !Param !(Maybe LCExp)
            | LCApp !LCExp !LCExp
            deriving (Eq)

instance Show LCExp where
    show (LCLabel l) = l
    show (LCLam param (Just body)) = "(\\" ++ param ++ "." ++ show body ++ ")"
    show (LCLam param (Nothing)) = "(\\" ++ param ++ "." ++ ")"
    show (LCApp e1 e2) = (show e1) ++ " " ++ (show e2)

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
replaceInWith param (LCLam lclParam (Just lclExp)) applied = 
    LCLam lclParam $ Just $ replaceInWith param lclExp applied 
replaceInWith param body applied = body