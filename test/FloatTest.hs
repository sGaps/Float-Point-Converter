module Main (main) where

import Binary.Float.Base
import Binary.Float
import Data.Ratio

main :: IO ()
main = do
    putStrLn "Testing Float str functions"
    putStrLn . showBinFloatUnixUnderlined . floatToBinary 100 $ (10::Double)
    putStrLn . showBinFloatUnixUnderlined . floatToBinary 100 $ (10.123::Float)
    putStrLn . showBinFloatUnixUnderlined . floatToBinary 100 $ (fromIntegral 15 + (1%30))

