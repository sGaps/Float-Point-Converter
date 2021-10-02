module Binary (
    -- *Others
    digits,

    -----------------------------------

    -- * Integers
    BinInteger,
    StepI,


    integerToBinary,
    integerToBinaryWithSteps,

    -----------------------------------

    -- * Fracctional/Float
    BinFloat,
    StepF,

    -- Conversion between data types.
    fromBinInteger,
    -- Conversion class
    IntoBinary(..),

    -----------------------------------
    
    -- * IEEE Float
    IEEEFormat(..),
    IEEEFloat,

    -- Conversion between data types.
    fromBinFloatWithSteps,
    fromBinFloat,
    -- Conversion class
    IntoIEEE(..),

    -----------------------------------

    -- * Show/String utilities

    -- Integers (show)
    showBinInteger,
    showsBinInteger,

    -- Float/Fracctional (show)
    showBinFloatSimple,
    showBinFloatUnixUnderlined,
    showsBinFloat,

    -- IEEE (show)
    showIEEE,
    showsIEEE,

    -- Steps
    showIntegerSteps,
    showFloatSteps,
    showNormalizedFloatSteps
) where

import Binary.Integer
import Binary.Float
import Binary.IEEE
import Binary.Show
    
