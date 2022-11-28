module OpenQASM.Model where

import OpenQASM.Types
import OpenQASM.Defines


data Metadata = Metadata 
    { version :: String
    }
    deriving (Show)

type Annotation = String

data Statement = Statement 
    { stateAnnots   :: [Annotation]
    , stateCall     :: Call
    }

type Identifier = String

data SumOp = AddOp 
           | SubOp
           deriving (Show)

data CompoundOp = CAddOp
                | CSubOp
                deriving (Show)

data Expression = ParenExp Expression
                | IndexExp Expression Integer -- This integer index needs to be changed to support ranges
                | AddExp   Expression Expression 
                | SubExp   Expression Expression
                | IdentExp Identifier
                deriving (Show)

data Call = AssignmentCall Identifier (Maybe CompoundOp) Expression

newtype Program = Program 
    ( Metadata
    , [Statement]
    )

instance Show (Call) where
    show (AssignmentCall id mop exp) = 
        id ++ " " ++ cop ++ assignTok ++ " " ++ show exp
        where cop = case mop of
                Just op -> show op
                Nothing -> ""  

    show _ = undefined

instance Show (Statement) where
    show (Statement{stateAnnots=stateAnnots, stateCall=stateCall}) = 
        annotStrings ++ (show stateCall)
        where annotStrings = 
                ( foldl (++) ""
                . map (++"\n")
                . map ("@"++)
                ) stateAnnots


instance Show (Program) where
    show (Program (md, statements)) = (
            (((\v -> "OPENQASM " ++ v ++ ";\n\n") $ version md)++)
            . foldl (++) ""
            . map (++";\n")
            . map show
        ) statements