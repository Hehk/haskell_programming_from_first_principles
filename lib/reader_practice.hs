module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]

y = [4, 5, 6]

z = [7, 8, 9]

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' = flip lookup $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

summed :: (Num c) => (c, c) -> c
summed = uncurry' (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (> 3) <*> (< 8)

fromMaybe' :: a -> Maybe a -> a
fromMaybe' _ (Just y) = y
fromMaybe' x _ = x

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7

prob2 = sequA $ fromMaybe' 0 s'
prob4 n = bolt <$> z' n 

exercise :: IO ()
exercise = do
  print $ foldr (&&) True (sequA 5)
  print $ foldr (&&) True (sequA 6)
  print $ prob2
  print $ bolt $ fromMaybe 0 ys
  print $ prob4 3
