module Binary.Show (
    -- * Defined Locally
    showGenericSteps,
    showIntegerSteps,
    showFloatSteps,
    showNormalizedFloatSteps,

    -- * Re-exports
    showBinInteger,
    showsBinInteger,

    showBinFloatSimple,
    showBinFloatUnixUnderlined,
    showsBinFloat,

    showIEEE,
    showsIEEE
) where

import Binary.Integer
import Binary.Float
import Binary.IEEE

import Text.Printf (printf)
import Data.List   (intersperse)
import Data.Ratio

-- | Renders a strings from a list of values.
showGenericSteps :: Show a
                => (a -> ShowS) -- ^ function used to render each element.
                -> Int          -- ^ indent
                -> [a]          -- ^ source list
                -> String
showGenericSteps showStep indent xs = render ""
    where ignore  = showString ""
          newline = showChar '\n'

          compose = foldr (.) ignore
          render  = compose             .
                    intersperse newline .
                    fmap repr $
                    (zip ([1..]::[Int]) xs)

          offset  = length . digits . length $ xs

          left ix = showString (printf "%*cStep %0*v: ("
                                indent ' '
                                offset ix)
          center  = showStep
          right   = showString ")"

          repr (ix,step) = left ix . center step . right

-- | Renders a string from a list of steps made to convert
-- an integral number into its binary representation.
showIntegerSteps indent =
    showGenericSteps showStepI indent
    where showStepI (StepI idiv idigit) =
            showString "Div = "   .
            shows      (idiv)     .
            showString ", "       .
            showString "Digit = " .
            shows      (idigit)

showFloatSteps = showNormalizedFloatSteps 1

-- | Renders a string from a list of steps made to convert
-- a fracctional number into its binary representation.
showNormalizedFloatSteps :: (Integral a , Integral b , Show b)
                            => a                    -- ^ normalization factor
                            -> Int                  -- ^ indent
                            -> [StepF (Rational) b] -- ^ source list
                            -> String
showNormalizedFloatSteps norm indent =
    showGenericSteps showStepF indent
    where factor = fromIntegral norm
          showRatio r =
            shows (numerator r * factor) .
            showString " % "             .
            shows (denominator r * factor )
          showStepF (StepF fmul fdig) =
            showString "Mult = "  .
            showRatio  fmul       .
            showString ", "       .
            showString "Digit = " .
            shows      fdig

