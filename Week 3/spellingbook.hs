import Data.Char
import Data.List

helper w
  | w == [] = ""
  | otherwise = (head w) : " is for " ++ w

speller :: [[Char]] -> [Char]
speller x = ((intercalate ", ") . map helper) x