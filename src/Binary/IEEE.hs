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

-- | Transforms a 'BinFloat' into a 'IEEEFloat' without rounding.
fromBinFloatWithSteps :: Integral b
                            => IEEEFormat 
                            -> F.BinFloat b
                            -> (IEEEFloat b , IntegerSteps b)
fromBinFloatWithSteps format (F.BinFloat s is fs cs tr) =
    let (biasedExponent,rawSignificand) =
            case break (== 1) rawDigits of                          -- finds the first '1' and split the digit-list.
                (zeroes,[]    ) -> (0,rawDigits)                    -- if it has only zeroes. Then the number is zero.
                (zeroes,rawSig) ->
                    let fracctionalBias = genericLength zeroes      -- move point to the right (2^-something)
                        integerBias     = genericLength is     - 1  -- move point to the left  (2^+something)
                        exponent        = exponentBias + integerBias - fracctionalBias
                    in ( exponent , rawSig )
        (binRawExponent,expSteps) = I.integerToBinaryWithSteps biasedExponent
        binRawExponentDigits      = I.getDigits binRawExponent

        exponentPadding           = take (exponentLength - length binRawExponentDigits) onlyZeroes
        binExponent               = exponentPadding <> (reverse binRawExponentDigits)

        significand = take significandLength .
                      tail .    -- discard the first one! it doesn't matter if the number has only zeroes.
                      zipWithMaybe sum0Default onlyZeroes $
                      rawSignificand <> trueCycle

        dummyEXP      = fromIntegral biasedExponent :: (Integral z, Show z) => z  -- TODO: DELETE
    in (IEEEFloat s binExponent significand format,
        expSteps)
                      
    where signLength  = 1                   -- sign always uses one bit.
          integerBits = reverse is          -- integer bits in the right order.
          fraccBits   = fs                  -- finite fractional bits.
          cycleBits   = cs                  -- cyclic & bounded fractional bits (comes after fraccBits).
          onlyZeroes  = [0,0..]             -- a bunch of zeroes
          trueCycle   = if null cycleBits   -- infinite/finite cyclic bits.
                            then []
                            else cycle cs        

          rawDigits       = integerBits <> fraccBits <> cycleBits
          sum0Default x y = maybe 0 id x + maybe 0 id y

          exponentValue      = exponentLength  - 1
          exponentBias       = 2^exponentValue - 1
          (exponentLength,
           significandLength) = case format of
                                    IEEESingle -> (8 ,23) -- significand: 23 bits explicit stored
                                    IEEEDouble -> (11,52) -- significand: 52 bits explicit stored

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

