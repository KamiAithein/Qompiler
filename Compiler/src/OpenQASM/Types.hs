{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module OpenQASM.Types where

import Data.Bits

data Register a = Register 
    { regType :: a
    , regSize :: Integer
    }

instance (Show a) => Show (Register a) where
    show reg = (show $ regType reg) ++ "[" ++ (show $ regSize reg) ++ "]" -- TODO!

data Angle = Angle 
    { angleSize :: Integer
    , angle     :: Integer
    }
    deriving (Show)


data DurUnit = NanoSecond
             | MicroSecond
             | USecond
             | MilliSecond
             | Second
             deriving (Show)

data Duration = Duration 
    { durUnit  :: DurUnit
    , durValue :: Float
    }
    deriving (Show)


-- Uninitialized values are None
data ScalarVal = TBool      (Maybe Bool) 
               | TInt       (Maybe Integer)
               | TUInt      (Maybe Integer)
               | TFloat     (Maybe Float)
               | TAngle     (Maybe Angle)
               | TBit       (Maybe Bool)
               | TDuration  (Maybe Duration)
               deriving (Show)

data VQubit = VQubit 
    { vQubitSize  :: Integer
    , vQubitValue :: Integer -- This is a stopgap measure
    }
    deriving (Show)

data PQubit = PQubit
    { pQubitVal :: Integer -- This is a stopgap measure
    }
    deriving (Show)

data Qubit = TVQubit VQubit 
           | TPQubit PQubit
           deriving (Show)

data Gate = Gate 
    { test :: Integer -- This is a stopgap measure
    }
    deriving (Show)


data QuantumVal = TQubit (Maybe Qubit)
                | TGate  (Maybe Gate)
                deriving (Show)
