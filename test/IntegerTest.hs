module Main (main) where

import Binary.Integer.Base
import Binary.Integer

main :: IO ()
main = do
    putStrLn "Testing Integer str functions"
    putStrLn $ showBinInteger (BinInteger Positive [0,1,0,0,1,1])
    putStrLn $ showBinInteger (BinInteger Negative [0,0,0,0,0,1])
    putStrLn $ showBinInteger (BinInteger Positive [0])
    putStrLn $ showBinInteger (BinInteger Positive ([]::[Int]))

    putStrLn . show . integerToBinary $ 0
    putStrLn . showBinInteger . integerToBinary $ 0

