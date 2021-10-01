module Main where

import qualified Binary (digits)

main :: IO ()
main = do
    putStrLn "Hello There"
    print . Binary.digits $ 1234
