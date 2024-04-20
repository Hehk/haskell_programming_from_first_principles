module Chapter10Scratch () where

import qualified Data.List as List

customFoldr :: (a -> b -> b) -> b -> [a] -> b
customFoldr _ acc [] = acc
customFoldr f acc (x : xs) = f x (customFoldr f acc xs)

customFoldl :: (b -> a -> b) -> b -> [a] -> b
customFoldl f acc xs = customFoldr (flip f) acc $ reverse xs
