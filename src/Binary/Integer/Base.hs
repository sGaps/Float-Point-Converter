module Binary.Integer.Base (
    -- Data types
    StepI(..),
    Sign(..),
    BinInteger(..),

    -- Sign operations:
    numToSign,
    signToNum
) where

-- | Pair that holds relevant information about the conversion steps (Integer)
data StepI a = StepI {
    getDiv   :: a,  -- ^ current state.
    getDigit :: a   -- ^ digit of the current step.
} deriving (Show,Eq,Ord)

-- | Represents the negative or positive bit of the digit.
data Sign = Negative | Positive deriving( Eq , Ord , Enum , Show , Bounded )

-- TODO: Implement Ord manually.
-- | Represents a binary integer number.
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
