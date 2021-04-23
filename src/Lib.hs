module Lib
    ( srCyrAlphabet
    , srLatinAlphabet
    , cyrToLatin
    , fromCyr
    ) where

import Data.List
import Control.Monad

srCyrAlphabet :: [Char]
srCyrAlphabet = 
    ['а', 'б', 'в', 'г', 'д', 'ђ', 'е', 'ж', 'з', 'и',
     'ј', 'к', 'л', 'љ', 'м', 'н', 'њ', 'о', 'п', 'р',
     'с', 'т', 'ћ', 'у', 'ф', 'х', 'ц', 'ч', 'џ', 'ш',
     'А', 'Б', 'В', 'Г', 'Д', 'Ђ', 'Е', 'Ж', 'З', 'И',
     'Ј', 'К', 'Л', 'Љ', 'М', 'Н', 'Њ', 'О', 'П', 'Р', 
     'С', 'Т', 'Ћ', 'У', 'Ф', 'Х', 'Ц', 'Ч', 'Џ', 'Ш']

srLatinAlphabet :: [Either Char (Char, Char)]
srLatinAlphabet = 
    [ Left 'a', Left 'b', Left 'v', Left 'g', Left 'd',
      Left 'đ', Left 'e', Left 'ž', Left 'z', Left 'i', 
      Left 'j', Left 'k', Left 'l', Right ('l', 'j'), Left 'm', 
      Left 'n', Right ('n', 'j'), Left 'o', Left 'p', Left 'r', 
      Left 's', Left 't', Left 'ć', Left 'u', Left 'f',
      Left 'h', Left 'c', Left 'č', Right ('d', 'ž'), Left 'š',
      Left 'A', Left 'B', Left 'V', Left 'G', Left 'D',
      Left 'Đ', Left 'E', Left 'Ž', Left 'Z', Left 'I',
      Left 'J', Left 'K', Left 'L', Right ('L', 'J'), Left 'M',
      Left 'N', Right ('N', 'J'), Left 'O', Left 'P', Left 'R',
      Left 'S', Left 'T', Left 'Ć', Left 'U', Left 'F',
      Left 'H', Left 'C', Left 'Č', Right ('D', 'ž'), Left 'Š'
    ]

fromCyr :: Char -> Either Char (Char, Char)
fromCyr c = case c `elemIndex` srCyrAlphabet of
              Nothing -> Left c
              Just idx -> srLatinAlphabet !! idx

toString :: Either Char (Char, Char) -> String
toString (Left c) = [c]
toString (Right (c1, c2)) = [c1, c2]

cyrToLatin :: String -> String
cyrToLatin = concatMap (toString . fromCyr)
