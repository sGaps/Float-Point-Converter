module Binary.Float (
    -- * Data Types
    Sign(..),
    StepF(..),
    BinFloat(..),

    -- * Conversion
    fromBinInteger,
    IntoBinary(..),

    ratioToBinary,
    ratioToBinaryWithSteps,

    -- * Show utilities
    showBinFloatSimple,
    showBinFloatUnixUnderlined,
    showsBinFloat
) where

import Binary.Float.Base
import Binary.Float.Show
import Common

import qualified Binary.Integer as I (BinInteger(..),integerToBinaryWithSteps,StepI)
import Data.List  (findIndices)
import Data.Ratio (approxRational,Ratio)

-- | Converts a 'BinInteger' into a 'BinFloat'.
fromBinInteger (I.BinInteger s bits) = BinFloat s bits [] [] (Just 0)

-- | Transforms a rational/fracctional number into binary
-- returns a tuple of:
-- `(finite-digit steps,cyclic-digit steps,zeroes steps)`
--
--  where each step is a tuple of (number state,result)
--
-- > if not . null zeroes, then the number was fully represented
fraccToBin prec fracc =
    let rawSteps = take (prec+1) . tail . iterate update $ (fracc,0)    -- takes one step more to determine if the number has been truncated. 
                                                                        -- if number has cyclic digits => number is always truncated.
        (steps,zeroes)  = span notZero rawSteps                         -- if there's a zero in zeroes, then the number was totally represented.
        cycleRange      = findACycle steps
        (finite,cyclic) = case cycleRange of
                            Just [low,high] -> (take low steps, take (high-low) . drop low $ steps)
                            Nothing         -> (steps,[])
    in (finite,cyclic,zeroes)
    where notZero        = (/= toRational 0) . fst
          update (num,_) =              -- apply as many multiplications as required.
            let next  = num * 2         -- double the number.
                digit = truncate next   -- and takes the 'extra' digit.
            in if digit > 0             -- in order to build the binary representation.
                then (next - 1, digit)
                else (next    , digit)

-- | Attempts to find the first two indices for all `x` in `xs`:
findACycle xs = (!!? 0) . fmap (take 2) . filter (`hasAtLeast` 2) . fmap ($ xs) . fmap findInterval $ xs
    where findInterval k = findIndices (k ==)

-- | Returns a tuple of
-- (Truncation index, finite-steps, cyclic-steps)
fractionalToBinary prec f =
    let (finite,cyclic,zeroes) = fraccToBin prec f
        truncated              = if null zeroes then Just prec else Nothing
        intoStepF              = fmap (uncurry StepF)
    in (truncated,(intoStepF finite) ++ (intoStepF . take 1 $ zeroes),intoStepF cyclic)


-- | Returns the binary representation of the given rational, based on the precision `prec`.
ratioToBinary prec ratio =
    let (float,istep,fstep,csteps) = ratioToBinaryWithSteps prec ratio  -- unpacking.
    in float

-- | Returns the binary representation of the given rational, based on the precision `prec`
-- and also returns the conversion steps of the integer, fractional part and the cyclic part.
ratioToBinaryWithSteps prec ratio =
    let integer    = truncate ratio
        fractional = ratio - (fromIntegral integer)

        -- integer number , conversion steps (integer)
        (binInt , integerSteps)             = I.integerToBinaryWithSteps integer   -- unpack

        -- truncation index , conversion steps (finite digits) , conversion steps (cyclic digits)
        (trunAt, finiteSteps , cyclicSteps) = fractionalToBinary prec fractional

        -- integer unpacking
        nsign   = I.getSign   binInt
        idigits = I.getDigits binInt
        
        -- floating context:
        fdigits = fmap getDigit finiteSteps
        cdigits = fmap getDigit cyclicSteps
    in (BinFloat nsign idigits fdigits cdigits trunAt,
        integerSteps,
        finiteSteps,
        cyclicSteps)

genericFloatToBinaryWithSteps epsilon prec float =
    let fracc = approxRational float epsilon
    in ratioToBinaryWithSteps prec fracc

genericFloatToBinary epsilon prec float =
    let (bin,_,_,_) = (genericFloatToBinaryWithSteps epsilon prec float)
    in bin

type FloatSteps   a b = [StepF a b]
type IntegerSteps a   = [I.StepI a]

-- | It defines two methods to convert a number into its binary representation
class RealFrac a => IntoBinary a where
    -- | Shared constant used to approximate the values to 'Rational'
    -- in the function 'floatToBinaryWithSteps'.
    approxEpsilon :: a

    -- | Transform a 'RealFrac' value into its Binary represntation.
    --
    -- returns 
    --
    -- @
    -- (BinFloat number,
    --  conversion steps of the integral part ,
    --  conversion steps of the fractional finite part ,
    --  conversion steps of the cyclic digits part )
    -- @
    floatToBinaryWithSteps :: Integral b 
                => Int 
                -> a 
                -> (BinFloat b,IntegerSteps b, FloatSteps Rational b, FloatSteps Rational b)
    floatToBinaryWithSteps = genericFloatToBinaryWithSteps approxEpsilon

    floatToBinary :: Integral b => Int -> a -> BinFloat b
    floatToBinary prec float =
        let (bin,_,_,_) = floatToBinaryWithSteps prec float
        in bin


instance IntoBinary Float where
    -- Error of the Float approximation (8 digits)
    approxEpsilon = epsilonFloat
                    where epsilonFloat = 0.00000005

instance IntoBinary Double where
    -- Error of the Double approximation (17 digits)
    approxEpsilon = epsilonDouble
                    where epsilonDouble = 0.00000000000000005
    
instance Integral a => IntoBinary (Ratio a) where
    -- No approximation needed
    approxEpsilon = identityEpsilon
                    where identityEpsilon = 0

