module Chapter12Scratch where

import Data.Char
import Data.String

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n + 2) else Nothing

notThe :: String -> Maybe String
notThe s = if s == "the" then Nothing else Just s

replaceThe :: String -> String
replaceThe s =
  case notThe s of
    Nothing -> "a"
    Just x -> x

countTheBeforeVowel :: String -> Int
countTheBeforeVowel s =
  case splitOn ' ' s of
    (a : b : rest) ->
      case (notThe a, isVowel $ head b) of
        (Nothing, Just True) -> 1 + countTheBeforeVowel (join " " rest)
        _ -> countTheBeforeVowel (join " " (b : rest))
    _ -> 0

join :: String -> [String] -> String
join _ [] = ""
join _ [x] = x
join sep (x : xs) = x ++ sep ++ join sep xs

isVowel :: Char -> Maybe Bool
isVowel c = if elem c "aeiou" then Just True else Just False

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, b) -> case b of
    [] -> [a]
    _ -> a : splitOn c (tail b)
