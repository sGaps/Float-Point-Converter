module Binary (
    -- Integers
    BinInteger,
    StepI,

    -- Integers (util)
    digits,

    -- Integers (show)
    showBinInteger,
    showsBinInteger,

    --------------------------
    -- conversion [Integer] --
    --------------------------
    integerToBinary,
    integerToBinaryWithSteps,


    -- Fracctional
    BinFloat,
    StepF,

    -- Show utilities
    showBinFloatSimple,
    showBinFloatUnixUnderlined,
    showsBinFloat,

    -----------------------------
    -- conversion [Fractional] --
    -----------------------------
    -- Conversion between data types.
    fromBinInteger,
    -- Conversion class
    IntoBinary(..),



    -- IEEE numbers
    IEEEFormat(..),
    IEEEFloat,


    showIEEE,
    showsIEEE,

    -----------------------
    -- conversion [IEEE] --
    -----------------------
    -- Conversion between data types.
    fromBinFloatWithSteps,
    fromBinFloat,
    -- Conversion class
    IntoIEEE(..)
) where

import Binary.Integer
import Binary.Float
import Binary.IEEE

