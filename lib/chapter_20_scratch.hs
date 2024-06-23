import Data.Monoid

data Identity a
  = Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

sum :: (Foldable t, Num a) => t a -> a
sum = foldl (+) 0

product :: (Foldable t, Num a) => t a -> a
product = foldl (*) 1

swap :: (a -> b -> c) -> b -> a -> c
swap f y x = f x y

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem y = foldl (swap $ (||) . (== y)) False

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum =
  foldl
    (swap $ fmap min . pure)
    Nothing

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum =
  foldr
    (pure max . pure)
    Nothing

-- null :: (Foldable t) => t a -> Bool
-- length :: (Foldable t) => t a -> Int
-- toList :: (Foldable t) => t a -> [a]
-- fold :: (Foldable t, Monoid m) => t m -> m
-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
