module Binary.Integer (
    -- * Utilities
    digits,
    evaluateBinInteger,
    fromDigitList,

    -- * Conversion
    integerToBinary,
    integerToBinaryWithSteps,

    -- * Data types
    Sign(..),
    StepI(..),
    BinInteger(..),

    -- * Sign operations
    numToSign,
    signToNum,

    -- * Show/String utilities
    showBinInteger,
    showsBinInteger
) where

import Binary.Integer.Base
import Binary.Integer.Show

-- | returns the digits of a given number k as a list of Integral numbers, where:
--
-- * the least significant digit is at the index `0`.
--
-- * the most significant digit is the last element of the list.
--
-- > digits number ! 0 = number `mod` 10
--
digits :: Integral a => a -> [a]
digits k
    | k < 10    = [k]
    | otherwise = let (q,r) = k `quotRem` 10
                  in q `seq` r : digits q

-- | Encode raw a binary-digit list into a Integral type.
--
-- The first element of the list is the least significant
-- digit and the last one is the most significant.
fromDigitList :: Num a => [a] -> a
fromDigitList = sum . zipWith (*) exps
    where exps = iterate (*2) 1

-- | Converts a Binary Integer into its regular form.
evaluateBinInteger :: Num a => BinInteger a -> a
evaluateBinInteger (BinInteger s ds) = (sign *) . sum . zipWith (*) exps $ ds
        where exps = iterate (*10) 1
              sign = signToNum s


-- | Returns the given number in its binary format.
integerToBinary :: Integral a => a -> BinInteger a
integerToBinary = (fst . integerToBinaryWithSteps)

-- | Represents a given Integral number in the binary
-- numerical system, and also returns the steps that
-- were made in the conversion process.
integerToBinaryWithSteps :: Integral a => a -> (BinInteger a, [StepI a])
integerToBinaryWithSteps k =
    if k == 0
        then (zero, trivial)
        else let sign  = numToSign k
                 bin   = tail . iterate division $ (abs k,0)
                 steps = takeWhile notZero bin
                 bits  = snd <$> steps
             in (BinInteger sign bits, uncurry StepI <$> steps)
    where notZero  = (/= (0,0))
          division = ((`quotRem` 2) . fst)
          trivial  = [StepI 0 0]
          zero     = BinInteger Positive [0]

