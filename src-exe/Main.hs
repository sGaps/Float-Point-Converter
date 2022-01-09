module Main where

import Binary (digits,
               showBinFloatUnixUnderlined,
               showIEEE,
               floatToBinary,
               IEEEFormat(..),
               floatToIEEEWithSteps,
               showIntegerSteps,
               showNormalizedFloatSteps)
import Binary.Integer (fromDigitList)
import Binary.IEEE    (IEEEFloat(..))
import Data.Ratio

-- Sample transformation functions:
makeSample   :: Integral a => Int -> a -> [a]
makeSample   k = take k . filter (/= 0) . digits

sumSample    :: Integral a => Int -> a -> a
sumSample    k = sum . makeSample k

concatSample :: Integral a => Int -> a -> a
concatSample k = foldr sum10 0 . makeSample k
            where x `sum10` y = let y' = 10 * y         -- <| it's likely to:
                                in y' `seq` (x + y')    -- (+) <$> (*10)

-- Sample data:
origin :: Integer
origin = 59903
sample = makeSample 4 origin
d      = sumSample  4 origin
x1     = 1 % (fromIntegral d)

e      = concatSample 4 origin
x2     = fromIntegral e + x1

main :: IO ()
main = do
    let indent     = 4
        floatLimit = 100
        actualRepr = fromRational x2 :: Float
        binFloat   = floatToBinary floatLimit x2     -- number
        (ieee,isteps,fsteps,csteps,esteps) =    -- unpack
            floatToIEEEWithSteps IEEESingle floatLimit x2
    putStrLn $ "--  Sample   --"
    putStrLn $ "Number:           " ++ show origin
    putStrLn $ "Sample:           " ++ show sample
    putStrLn $ "Digit Sum (d):    " ++ show d
    putStrLn $ "Digit Concat (e): " ++ show e
    putStrLn $ "-- variables --"
    putStrLn $ "x1 = (1/d)    = " ++ show x1
    putStrLn $ "x2 = (e + x1) = " ++ show x2
    putStrLn $ ""

    putStrLn $ "-- Results --"
    putStrLn $ "Float number:  " ++ show actualRepr
    putStrLn $ "Binary Number: " ++ showBinFloatUnixUnderlined binFloat
    putStrLn $ "IEEE Number:   " ++ showIEEE ieee
    putStrLn $ " > sign:                 " ++ show (getSign ieee)
    putStrLn $ " > biased exponent:      " ++ show (fromDigitList . reverse . getExponent $ ieee)
    putStrLn $ " > decoded significand : " ++ show (fromDigitList . reverse . getSignificand $ ieee)
    putStrLn $ ""

    putStrLn $ "-- Procedure --"
    putStrLn $ "Integer Part's Conversion."
    putStrLn $ showIntegerSteps indent isteps
    putStrLn $ ""

    putStrLn $ "Finite Fractional Digits to Binary."
    putStrLn $ showNormalizedFloatSteps 2 indent fsteps
    putStrLn $ ""

    putStrLn $ "Cyclic Fractional Digits to Binary."
    putStrLn $ showNormalizedFloatSteps 2 indent csteps
    putStrLn $ ""

    putStrLn $ "Biased Exponent to Binary."
    putStrLn $ showIntegerSteps indent esteps
    putStrLn $ ""

