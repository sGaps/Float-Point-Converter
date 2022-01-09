module Binary.Float.Base (
    Sign(..),
    StepF(..),
    BinFloat(..)
) where

import Binary.Integer.Base (Sign(..))

-- | Pair that holds relevant information about the conversion steps (Fractional)
data StepF a b = StepF {
    getMult  :: a,  -- ^ current state.
    getDigit :: b   -- ^ digit of the current step.
} deriving ( Show , Eq , Ord )

-- | Represents a number with a integer and a fractional part.
-- It also holds information about the digits that form an
-- infinite digit cycle.
--
-- When the precision is good enough for finding a digit cycle,
-- 'truncIndex' must have the value 'Nothing'. otherwise, the
-- precision that was used must be specified in that field.
--
-- === Encoding
--
-- Given a decimal number `x`, we can represent it in the
-- binary numerical system as follows:
--
-- @
--   | integer part            | fractional part
--                                               _______________
-- x = b_n b_{n-1} ··· b_1 b_0 . e_0 e_1 ··· e_m e_{m+1} e_{m+2} ···
--                               ^           ^   ^
--                               |           |   |
--                               |           |   cycleDigits
--                               fraccDigits-'
-- @
--
-- where each b_i, e_j \in [0,1] and i, j \in \Nat.
--
-- But we're unable to represent all the digits in the computer,
-- so we can hold as much information as we need in the BinFloat
-- type.
--
-- Following the previous form of `x`, we can represent it
-- in haskell like this:
--
-- @
-- BinFloat { sign          = number_sign
--            integerDigits = [b_0, b_1, ... b_n]
--            fraccDigits   = [e_0, b_1, ... e_m]
--            cyclic        = [e_{m+1}, e_{m+2}, ...]
--            truncIndex    = Just m
--           }
-- @
--
data BinFloat a = BinFloat {
    sign          :: Sign ,     -- ^ sign of the number.
    integerDigits :: [a]  ,     -- ^ integer part of the number.
    fraccDigits   :: [a]  ,     -- ^ digits of the finite fracctional part.
    cycleDigits   :: [a]  ,     -- ^ digits of the cyclic digits after the finite fracctional part.
    truncIndex    :: Maybe Int  -- ^ truncation index.
} deriving (Show,Eq)


--data ConversionSteps a = ConversionSteps {
--                                getIntegerSteps :: [StepI a]
--                                getFiniteSteps  :: [StepF a]
--                                getCyclicSteps  :: [StepF a]
--                            }

