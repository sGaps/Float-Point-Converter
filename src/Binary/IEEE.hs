module Binary.IEEE (
    -- * Re-exports
    IEEEFormat(..),
    IEEEFloat(..),

    -- * Conversion
    fromBinFloatWithSteps,
    fromBinFloat,

    -- * Data types
    IntoIEEE(..),

    -- * Show/String utilities
    showIEEE,
    showsIEEE
) where

import Binary.IEEE.Base
import Binary.IEEE.Show
import Common

import qualified Binary.Float   as F (StepF,BinFloat(..),IntoBinary(..))
import qualified Binary.Integer as I (StepI,BinInteger(..),integerToBinaryWithSteps)

import Data.List  (findIndex,genericLength)
import Data.Ratio (Ratio)

type FloatSteps   a b = [F.StepF a b]
type IntegerSteps a   = [I.StepI a  ]


applyBias format intbits fracbits cyclbits =
    let (zeroes, startsWithOne) = break (== 1) significandDigits
        fracctionalBias = genericLength zeroes      -- move floating point to the right (2^-something)
        integerBias     = genericLength intbits - 1 -- move floating point to the left  (2^+something)
        biasedExponent  = if null startsWithOne     -- If doesn't have any one, the exponent must be zero.
                           then 0
                           else exponentBias + integerBias - fracctionalBias
    in (biasedExponent , startsWithOne)
    where bitsUsed          = exponentSize format
          effectiveBits     = bitsUsed        - 1 -- The domain of the bit field is sliced at the middle
          exponentBias      = 2^effectiveBits - 1 -- so, we have to use the fixed bias.
          significandDigits = intbits <> fracbits <> cyclbits

-- TODO: Change this to accept an integer instead of a format, so we can apply rounding later.
fixedSignificand format significand =
        take significandLength .
        tail .                              -- The IEEE's floating point format implicitly saves the first
        zipWith (+) [0,0..] $               -- digit 1 when the exponent is not zero, so we can ignore it.
                    (significand <> [0,0..])
    where significandLength = significandSize format
    

-- | Transforms a 'BinFloat' into a 'IEEEFloat' without rounding.
fromBinFloatWithSteps :: Integral b
                            => IEEEFormat 
                            -> F.BinFloat b
                            -> (IEEEFloat b , IntegerSteps b)
fromBinFloatWithSteps format
                      (F.BinFloat sign
                                  intBits   -- form: [b0, ... bn]
                                  fraccBits
                                  cycleBits
                                  _) =
    let (biasedExponent, biasedSignificand) = applyBias format
                                                        integerBits
                                                        fraccBits
                                                        cycleBits

        (binIntExponent,expSteps) = I.integerToBinaryWithSteps biasedExponent
        exponentDigits            = (reverse . I.getDigits) binIntExponent

        exponentPadding = take (exponentLength - length exponentDigits) [0,0..]
        binExponent     = exponentPadding <> exponentDigits

        significand = fixedSignificand format (biasedSignificand <> tailCycle)
    in (IEEEFloat sign binExponent significand format,
        expSteps)
                      
    where integerBits     = reverse intBits -- integer bits in the right order. [bn, ... b0]
          exponentLength  = exponentSize format
          tailCycle   = if null cycleBits   -- infinite/finite cyclic bits.
                            then []
                            else cycle cycleBits


-- | Works like `fromBinFloatWithSteps` but discards the conversion steps.
fromBinFloat format = fst . fromBinFloatWithSteps format

-- | Common interface to convert numbers into
-- 'IEEEFloat' without rounding.
class F.IntoBinary a => IntoIEEE a where
    -- | Default IEEE format used to convert a given number into
    -- 'IEEEFloat'
    defaultCastType      :: a -> IEEEFormat

    -- | Transforms a value into IEEE and returns it with its conversion steps.
    --
    -- returns
    --
    -- @
    -- (IEEE number,
    --  conversion steps of the integral part ,
    --  conversion steps of the fractional finite part ,
    --  conversion steps of the cyclic digits part ,
    --  conversion steps of the integral exponent part )
    -- @
    floatToIEEEWithSteps :: Integral b => IEEEFormat                -- ^ format to represent the given number.
                                       -> Int                       -- ^ BinFloat depth or bit-precision.
                                       -> a                         -- ^ number to convert.
                                       -> (IEEEFloat b,
                                           IntegerSteps b,
                                           FloatSteps Rational b,
                                           FloatSteps Rational b,
                                           IntegerSteps b)
    floatToIEEEWithSteps format prec number = 
        let (bin,isteps,fsteps,csteps) = F.floatToBinaryWithSteps prec number
            (ieee,esteps)              = fromBinFloatWithSteps format bin
        in (ieee,isteps,fsteps,csteps,esteps)

    -- | Transforms a value into a IEEE number.
    --
    -- See also: 'floatToIEEEWithSteps'.
    floatToIEEE :: Integral b => IEEEFormat     -- ^ format to represent the given number.
                              -> Int            -- ^ BinFloat depth or bit-precision.
                              -> a              -- ^ number to convert.
                              -> IEEEFloat b
    floatToIEEE format prec number =
        let (ieee,_,_,_,_) = floatToIEEEWithSteps format prec number
        in ieee

    -- | Transforms a value into a IEEE number and returns it with its conversion steps.
    --
    -- __NOTE:__ this uses the format returned by 'defaultCastType'
    --
    -- See also: 'floatToIEEEWithSteps'.
    floatToDefaultIEEEWithSteps :: Integral b 
                                    => Int                      -- ^ BinFloat depth or bit-precision.
                                    -> a                        -- ^ number to convert.
                                    -> (IEEEFloat b,
                                        IntegerSteps b,
                                        FloatSteps Rational b,
                                        FloatSteps Rational b,
                                        IntegerSteps b)
    floatToDefaultIEEEWithSteps prec number =
        floatToIEEEWithSteps defaultCast prec number
            where defaultCast = defaultCastType number

    -- | Transforms a value into IEEE
    --
    -- __NOTE:__ this uses the format returned by 'defaultCastType'
    --
    -- See also: 'floatToIEEEWithSteps'.
    floatToDefaultIEEE :: Integral b => Int         -- ^ BinFloat depth or bit-precision.
                                     -> a           -- ^ number to convert.
                                     -> IEEEFloat b
    floatToDefaultIEEE prec number =
        floatToIEEE defaultCast prec number
            where defaultCast = defaultCastType number

instance IntoIEEE Double where
    defaultCastType _ = IEEEDouble

instance IntoIEEE Float where
    defaultCastType _ = IEEESingle

instance Integral a => IntoIEEE (Ratio a) where
    defaultCastType _ = IEEEDouble

