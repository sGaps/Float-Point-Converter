module Binary.Integer.Base (
    -- * Data types
    StepI(..),
    Sign(..),
    BinInteger(..),
    -- * Sign operations
    numToSign,
    signToNum
) where

-- | Pair that holds relevant information about the conversion steps (Integer)
data StepI a = StepI {
    getDiv   :: a,  -- ^ current state.
    getDigit :: a   -- ^ digit of the current step.
} deriving (Show,Eq,Ord)

-- | Represents the negative or positive bit of the number.
data Sign = Negative | Positive deriving( Eq , Ord , Enum , Show , Bounded )

-- | Represents a binary integer number.
--
-- This data type is used to hold both sign and binary digits in
-- a general form.
--
-- === Encoding
--
-- Given some integer number `b` that can be represented in
-- the numerical binary system by the digits:
--
-- b = b_n b_{n-1} ... b_2 b_1 b_0
--
-- /(and b_0, b_1, ... b-n \in \{0,1\}.)/
--
-- A BinInteger instance can represent `b` in a digit its
-- internal list field where each b_i is ordered by
-- its exponent/magnitude.
--
-- In that sense, a valid BinInteger have the form of:
--
-- @
-- BinInteger sign [b_0, b_1, ... bn]
-- @
-- 
data BinInteger a = BinInteger {
    getSign   :: Sign,  -- ^ holds de value of the digit.
    getDigits :: [a]    -- ^ holds the binary digits of the number.
} deriving( Eq , Show )

-- | Casts a number into a Sign enum.
numToSign :: (Num a , Ord a) => a -> Sign
numToSign k = if signum k < 0
                then Negative
                else Positive

-- | Casts a sign into a Num instance.
signToNum :: Num a => Sign -> a
signToNum Positive = 1
signToNum Negative = -1

