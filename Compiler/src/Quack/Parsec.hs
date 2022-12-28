module Quack.Parsec where

import Quack.Common

import Control.Monad
import Data.List

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Char


lcProgram :: GenParser Char st LCProgram
lcProgram = do
    { defs <- lcDef `endBy` char ';'
    ; body <- lcSS
    ; return $ LCProgram
        { lcpEnv = defs
        , lcpExp = body
        }
    }

lcDef :: GenParser Char st Definition
lcDef = do
    { char '['
    ; defined <- many letter
    ; char ']'
    ; char '>'
    ; body <- lcSS
    ; return (defined, body)
    }

-- follow
lcSS :: GenParser Char st LCExp
lcSS = do 
    { head <- lcS
    ; tailExps <- optionMaybe $ many1 lcS
    ; let sExp = case tailExps of
            Nothing -> head
            Just exps -> foldl' (\acc new -> LCApp acc new) head exps 
    ; return sExp
    }

lcS :: GenParser Char st LCExp
lcS = do
    { space
    ; ret <- lcS
    ; return ret
    }
    <|> do
    { char '('
    ; exp <- lcP
    ; return exp
    }
    <|> do
    { label <- many1 letter
    ; return $ LCLabel label
    }
    <|> do 
    { lam <- lcLam
    ; return lam
    }

lcLam :: GenParser Char st LCExp
lcLam = do
    { char '\\'
    ; param <- many letter
    ; char '.'
    ; body <- optionMaybe lcSS
    ; return $ LCLam param body 
    }

lcP :: GenParser Char st LCExp
lcP = do
    { lam <- lcLam
    ; char ')'
    ; return lam 
    }
    <|>
      do
    { pBody <- lcSS
    ; char ')'
    ; return pBody
    }