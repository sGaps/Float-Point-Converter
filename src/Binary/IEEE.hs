module Binary.IEEE (
    IEEEFormat(..),
    IEEEFloat(..),

    fromBinFloatWithSteps,
    fromBinFloat,

    IntoIEEE(..),

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

-- TODO: Remove
import Debug.Trace


type FloatSteps   a b = [F.StepF a b]
type IntegerSteps a   = [I.StepI a  ]

-- | Truncates, it doesn't apply rounding
fromBinFloatWithSteps :: Integral b
                            => IEEEFormat 
                            -> F.BinFloat b
                            -> (IEEEFloat b , IntegerSteps b)
fromBinFloatWithSteps format (F.BinFloat s is fs cs tr) =
    let (biasedExponent,rawSignificand) =
            case break (== 1) rawDigits of
                (zeroes,[]    ) -> (0,rawDigits)
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

          -- number f
          rawDigits       = integerBits <> fraccBits <> cycleBits
          sum0Default x y = maybe 0 id x + maybe 0 id y

          -- TODO: Change for span (== 1)
          splitAtFirstOne xs = findIndex (==1) xs >>= return . (flip splitAt) xs

          exponentValue      = exponentLength  - 1
          exponentBias       = 2^exponentValue - 1
          (exponentLength,
           significandLength) = case format of
                                    IEEESingle -> (8 ,23) -- significand: 23 bits explicit stored
                                    IEEEDouble -> (11,52) -- significand: 52 bits explicit stored

fromBinFloat format = fst . fromBinFloatWithSteps format

class F.IntoBinary a => IntoIEEE a where
    defaultCastType      :: a -> IEEEFormat

    floatToIEEEWithSteps :: Integral b => IEEEFormat                -- ^ format to represent the given numer
                                       -> Int                       -- ^ precision (# of division steps when converting into a binary fractional number)
                                       -> a                         -- ^ number to convert
                                       -> (IEEEFloat b,             -- ^ New Representation,
                                           IntegerSteps b,          --   Steps of the conversion of the integral part of the number
                                           FloatSteps Rational b,   --   Steps of the conversion of the fractional finite part
                                           FloatSteps Rational b,   --   Steps of the conversion of the cyclic part
                                           IntegerSteps b)          --   Steps of the conversion of the exponent
    floatToIEEEWithSteps format prec number = 
        let (bin,isteps,fsteps,csteps) = F.floatToBinaryWithSteps prec number
            (ieee,esteps)              = fromBinFloatWithSteps format bin
        in (ieee,isteps,fsteps,csteps,esteps)

    floatToIEEE :: Integral b => IEEEFormat     -- ^ format to represent the given numer
                              -> Int            -- ^ precision (# of division steps when converting into a binary fractional number)
                              -> a              -- ^ number to convert
                              -> IEEEFloat b    -- ^ New Representation,
    floatToIEEE format prec number =
        let (ieee,_,_,_,_) = floatToIEEEWithSteps format prec number
        in ieee

    floatToDefaultIEEEWithSteps :: Integral b 
                                    => Int                      -- ^ precision (# of division steps when converting into a binary fractional number)
                                    -> a                        -- ^ number to convert
                                    -> (IEEEFloat b,            -- ^ New Representation,
                                        IntegerSteps b,         --   Steps of the conversion of the integral part of the number
                                        FloatSteps Rational b,    --   Steps of the conversion of the fractional finite part
                                        FloatSteps Rational b,    --   Steps of the conversion of the cyclic part
                                        IntegerSteps b)         --   Steps of the conversion of the exponent
    floatToDefaultIEEEWithSteps prec number =
        floatToIEEEWithSteps defaultCast prec number
            where defaultCast = defaultCastType number

    floatToDefaultIEEE :: Integral b => Int         -- ^ precision (# of division steps when converting into a binary fractional number)
                                     -> a           -- ^ number to convert
                                     -> IEEEFloat b -- ^ New Representation,
    floatToDefaultIEEE prec number =
        floatToIEEE defaultCast prec number
            where defaultCast = defaultCastType number

instance IntoIEEE Double where
    defaultCastType _ = IEEEDouble

instance IntoIEEE Float where
    defaultCastType _ = IEEESingle

instance Integral a => IntoIEEE (Ratio a) where
    defaultCastType _ = IEEEDouble

