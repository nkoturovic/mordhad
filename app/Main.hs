module Main where

import qualified SerbianAlphabet as SRB

main :: IO ()
main = do 
    putStrLn $ SRB.cyrToLatin "Здраво свете!!"
