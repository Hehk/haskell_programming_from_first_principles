module Cipher where

import Data.Char

fitToRange :: Int -> Int -> Int -> Int
fitToRange x start end =
  let range = end - start + 1
      offset = x - start
   in start + ((offset) `mod` (range))

ceasarChar :: Int -> Char -> Char
ceasarChar n c =
  let newC = (ord c) + n
      start = if isLower c then ord 'a' else ord 'A'
      end = if isLower c then ord 'z' else ord 'Z'
   in chr $ fitToRange newC start end

ceasarCipher :: Int -> String -> String
ceasarCipher _ "" = []
ceasarCipher n (x : xs) =
  ceasarChar n x : ceasarCipher n xs

offset :: Char -> Int
offset c = ord c - (if isLower c then ord 'a' else ord 'A')

vigenereCipher :: String -> String -> (String, [Int])
vigenereCipher message keyword =
    foldl
      ( \(acc, offsets) c ->
          if isSpace c
            then (acc ++ [c], offsets)
            else
              let n = head offsets
                  rest = tail offsets
                  newC = ceasarChar n c
               in (acc ++ [newC], rest)
      )
      ("", take 100 $ cycle $ map offset keyword)
      message
