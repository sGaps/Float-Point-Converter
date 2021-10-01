module Binary.Integer (
    -- Utilities:
    digits,
    integerToBinary,
    integerToBinaryWithSteps,

    -- Data types
    Sign(..),
    StepI(..),
    BinInteger(..),

    -- Sign operations:
    numToSign,
    signToNum,

    -- Show/String utilities
    showBinInteger,
    showsBinInteger
) where

import Binary.Integer.Base
import Binary.Integer.Show

-- | returns the digits of a given number k as a list of Integral numbers, where:
-- * the least significant digit is at the index `0`:
-- * the most significant digit is the last element of the list.
--
-- > digits number ! 0 = number `mod` 10
--
digits :: Integral a => a -> [a]
digits k
    | k < 10    = [k]
    | otherwise = let (q,r) = k `quotRem` 10
                  in q `seq` r : digits q

-- | Converts a Binary Integer into its regular form.
evaluate (BinInteger s ds) = (sign *) . sum . zipWith (*) exps $ ds
        where exps = iterate (*10) 1
              sign = signToNum s


-- TODO: check this value against zero!
-- | Returns the given number in a binary format.
integerToBinary = (fst . integerToBinaryWithSteps)

-- | Returns an integer in its binary format with the conversion steps.
integerToBinaryWithSteps :: Integral a => a -> (BinInteger a, [StepI a])
integerToBinaryWithSteps k =
    if k == 0
        then zero
        else let sign  = numToSign k
                 bin   = tail . iterate update $ (abs k,0)
                 steps = takeWhile notZero bin
                 bits  = snd <$> steps
             in (BinInteger sign bits,uncurry StepI <$> steps)
    where notZero = (/= (0,0))
          update  = ((`quotRem` 2) . fst)
          zero    = (BinInteger Positive [0], [StepI 0 0])

