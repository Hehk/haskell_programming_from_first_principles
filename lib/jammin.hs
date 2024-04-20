module Jammin () where

import Data.List.NonEmpty

data Fruit
  = Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show, Ord)

data JamJars
  = Jam {fruit :: Fruit, jars :: Int}
  deriving (Eq, Show, Ord)

-- | JamJars| = |Fruit| * |Int|
--  |Fruit| = 4
row1 = Jam Peach 10

row2 = Jam Plum 20

row3 = Jam Apple 30

row4 = Jam Blackberry 40

row5 = Jam Peach 50

row6 = Jam Plum 60

allJam = [row1, row2, row3, row4, row5, row6]

jamTotal :: [JamJars] -> Int
jamTotal xs = sum $ Prelude.map jars xs

maxJam :: JamJars -> JamJars -> JamJars
maxJam a b = if jars a > jars b then a else b

mostRow :: NonEmpty JamJars -> JamJars
mostRow (x :| xs) = foldr maxJam x xs
