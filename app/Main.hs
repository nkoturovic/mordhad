module Main where

import qualified SerbianAlphabet as SRB

main :: IO ()
main = do 
    putStrLn $ SRB.latinToAsciiForm $ SRB.cyrToLatin "Здраво свете, студенти и ђаци!!"
