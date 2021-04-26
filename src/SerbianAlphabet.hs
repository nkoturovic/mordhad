module SerbianAlphabet
    ( cyrToLatin
    , latinToAsciiForm
    ) where

import Data.Foldable
import qualified Data.Vector as VEC

data LatinLetter = One Char 
                 | Two (Char, Char)
    deriving (Eq)

latinLetterStr :: LatinLetter -> String
latinLetterStr (One c) = [c]
latinLetterStr (Two (c1, c2)) = [c1, c2]

latinFromChar :: Char -> Maybe LatinLetter
latinFromChar c = find (== One c) latinAlphabet

latinFromChars :: (Char, Char) -> Maybe LatinLetter
latinFromChars cs = find (== Two cs) latinAlphabet

latinToEither :: LatinLetter -> Either Char (Char, Char)
latinToEither (One c) = Left c
latinToEither (Two cs) = Right cs

newtype CyrLetter = Cyr Char
    deriving (Eq)

cyrFromChar :: Char -> Maybe CyrLetter
cyrFromChar c = find (== Cyr c) cyrAlphabet

cyrAlphabet :: VEC.Vector CyrLetter
cyrAlphabet = VEC.fromList
    [Cyr 'а', Cyr 'б', Cyr 'в', Cyr 'г', Cyr 'д',
     Cyr 'ђ', Cyr 'е', Cyr 'ж', Cyr 'з', Cyr 'и',
     Cyr 'ј', Cyr 'к', Cyr 'л', Cyr 'љ', Cyr 'м', 
     Cyr 'н', Cyr 'њ', Cyr 'о', Cyr 'п', Cyr 'р',
     Cyr 'с', Cyr 'т', Cyr 'ћ', Cyr 'у', Cyr 'ф',
     Cyr 'х', Cyr 'ц', Cyr 'ч', Cyr 'џ', Cyr 'ш',
     Cyr 'А', Cyr 'Б', Cyr 'В', Cyr 'Г', Cyr 'Д',
     Cyr 'Ђ', Cyr 'Е', Cyr 'Ж', Cyr 'З', Cyr 'И',
     Cyr 'Ј', Cyr 'К', Cyr 'Л', Cyr 'Љ', Cyr 'М', 
     Cyr 'Н', Cyr 'Њ', Cyr 'О', Cyr 'П', Cyr 'Р', 
     Cyr 'С', Cyr 'Т', Cyr 'Ћ', Cyr 'У', Cyr 'Ф', 
     Cyr 'Х', Cyr 'Ц', Cyr 'Ч', Cyr 'Џ', Cyr 'Ш']

latinAlphabet :: VEC.Vector LatinLetter
latinAlphabet = VEC.fromList
    [One 'a', One 'b', One 'v', One 'g', One 'd',
     One 'đ', One 'e', One 'ž', One 'z', One 'i', 
     One 'j', One 'k', One 'l', Two ('l', 'j'), One 'm', 
     One 'n', Two ('n', 'j'), One 'o', One 'p', One 'r', 
     One 's', One 't', One 'ć', One 'u', One 'f',
     One 'h', One 'c', One 'č', Two ('d', 'ž'), One 'š',
     One 'A', One 'B', One 'V', One 'G', One 'D',
     One 'Đ', One 'E', One 'Ž', One 'Z', One 'I',
     One 'J', One 'K', One 'L', Two ('L', 'J'), One 'M',
     One 'N', Two ('N', 'J'), One 'O', One 'P', One 'R',
     One 'S', One 'T', One 'Ć', One 'U', One 'F',
     One 'H', One 'C', One 'Č', Two ('D', 'Ž'), One 'Š']

cyrToLatinLetter :: CyrLetter -> LatinLetter
cyrToLatinLetter (Cyr c) = case Cyr c `VEC.elemIndex` cyrAlphabet of
              Nothing -> One c
              Just idx -> latinAlphabet VEC.! idx

cyrToLatin :: String -> String 
cyrToLatin = concatMap $
    \x -> case cyrFromChar x of
        Nothing -> [x]
        Just l -> latinLetterStr $ cyrToLatinLetter l

latinAsciiPairs :: VEC.Vector (LatinLetter, Either Char (Char, Char))
latinAsciiPairs = VEC.fromList 
    [(One 'đ', Right ('d', 'j')), (One 'Đ', Right ('D', 'J')), (One 'ž', Left 'z'), (One 'Ž', Left 'Ž'),
     (One 'ć', Left 'c'), (One 'Ć', Left 'C'), (One 'č', Left 'c'), (One 'Č', Left 'C'), (Two ('d', 'ž'), 
     Right ('d', 'z')), (Two ('D', 'Ž'), Right ('D', 'Z')), (One 'š', Left 'š'), (One 'Š', Left 'Š')]

latinLetterToAsciiForm :: LatinLetter -> Either Char (Char, Char)
latinLetterToAsciiForm c = 
    case (\x -> c == fst x) `find` latinAsciiPairs of
      Just (_, a) -> a
      Nothing -> latinToEither c 

latinToAsciiForm :: String -> String
latinToAsciiForm = concatMap $
    \c -> case latinFromChar c of
             Nothing -> [c]
             Just l -> case latinLetterToAsciiForm l of 
                           Left c1 -> [c1]
                           Right (c1, c2) -> [c1, c2]
