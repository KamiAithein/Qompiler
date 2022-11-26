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
    deriving (Show)

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
          | ConstDeclCall  Type Identifier
          deriving (Show)

newtype Program = Program 
    ( Metadata
    , [Statement]
    )


instance Show (Program) where
    show (Program (md, statements)) = (
            (((\v -> "OPENQASM " ++ v ++ ";\n\n") $ version md)++)
            . unwords
            . map (\w -> (show w) ++ ";\n")
        )
        statements