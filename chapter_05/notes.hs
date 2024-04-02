module Notes where

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b

f :: Num a => a -> a -> a
f x y = x + y + 3

