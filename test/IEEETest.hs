module Main (main) where

import Binary.IEEE.Base
import Binary.IEEE
import Data.Ratio

main :: IO ()
main = do
    putStrLn "Testing IEEE str functions (NOTE: this doesn't apply rounding)"
    putStrLn . showIEEE . floatToDefaultIEEE 100 $ (10::Double)
    putStrLn . showIEEE . floatToDefaultIEEE 100 $ (10.123::Float)
    putStrLn . showIEEE . floatToDefaultIEEE 100 $ (fromIntegral 15 + (1%30))
    putStrLn "-- Custom show -- (NOTE: this doesn't apply rounding)"
    putStrLn . showIEEE . floatToIEEE IEEESingle 100 $ (fromIntegral 15 + (1%30))

