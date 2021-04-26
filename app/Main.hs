module Main where

import Control.Monad

import Options.Applicative
import Data.Semigroup ((<>))

import qualified SerbianAlphabet as SRB

data Options = Options
  { optCyrToLatin :: Bool
  , optLatinToAscii :: Bool 
  , optOutFile :: String 
  , optInFile :: String }

cyrToLatinOpt :: Parser Bool
cyrToLatinOpt = switch (long "cyr-to-latin" 
                      <> short 'l' <> help "Convert Sr-Cyrilic letters to Sr-Latin")

latinToAsciiOpt :: Parser Bool
latinToAsciiOpt = switch (long "latin-to-ascii" <> short 'a' 
                       <> help "Convert Sr-Latin letters to their ASCII form")

outFileOpt :: Parser String
outFileOpt = strOption (long "out" <> short 'o' <> metavar "FILE"
                     <> value "" <> help "Write program output to FILE")

inFileOpt :: Parser String
inFileOpt = strOption (long "in" <> short 'i' <> metavar "FILE"
                     <> value "" <> help "Read input from file instead of stdin")

opts :: ParserInfo Options
opts = info (Options <$> cyrToLatinOpt <*> latinToAsciiOpt
                     <*> outFileOpt <*> inFileOpt <**> helper)
  ( fullDesc
  <> progDesc "Transformating letters (line by line) according to options"
  <> header "MordHad - Word manipulation software")

genTransform :: Options -> (String -> String)
genTransform options = 
    (if optLatinToAscii options then SRB.latinToAsciiForm else id)
  . (if optCyrToLatin options then SRB.cyrToLatin else id)

main :: IO ()
main = do 
    options <- execParser opts
    let transform = genTransform options
    input <- if optInFile options /= "" 
                    then readFile $ optInFile options
                    else getContents

    let outFn = if optOutFile options /= "" 
                    then writeFile $ optOutFile options
                    else putStrLn

    outFn $ transform input
