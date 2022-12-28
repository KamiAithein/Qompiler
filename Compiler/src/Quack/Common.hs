module Quack.Common where

import qualified Data.HashMap.Strict as H


type Label = String
type Param = String
data LCExp  = LCLabel !Label
            | LCLam !Param !(Maybe LCExp)
            | LCApp !LCExp !LCExp
            deriving (Show)

type Definition = (Label, LCExp)
data LCProgram = LCProgram 
    { lcpEnv :: ![Definition]
    , lcpExp :: !LCExp
    }
    deriving (Show)