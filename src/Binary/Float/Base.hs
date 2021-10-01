module Binary.Float.Base (
    Sign(..),
    StepF(..),
    BinFloat(..)
) where

import Binary.Integer.Base (Sign(..))

-- | Pair that holds relevant information about the conversion steps (Fracctional)
data StepF a b = StepF {
    getMult  :: a,  -- ^ current state.
    getDigit :: b   -- ^ digit of the current step.
} deriving ( Show , Eq , Ord )

-- | Represents a number with integer and fracctional part.
--  It also holds information about the cyclic digits and the
--  truncation index.
data BinFloat a = BinFloat {
    sign          :: Sign ,     -- ^ sign of the number.
    integerDigits :: [a]  ,     -- ^ integer part of the number.
    fraccDigits   :: [a]  ,     -- ^ digits of the finite fracctional part.
    cycleDigits   :: [a]  ,     -- ^ digits of the cyclic digits after the finite fracctional part.
    truncIndex    :: Maybe Int  -- ^ truncation index.
} deriving (Show,Eq)

