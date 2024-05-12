import Control.Monad (join)

bind :: (Monad m) => (a -> m b) -> m a -> m b
bind f = join . fmap f

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
  putStrLn "blah"
    >> putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' =
  putStrLn "blah"
    *> putStrLn "another thing"

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else []

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second y) = Second (f y)
  fmap _ (First x) = First x

instance Applicative (Sum a) where
  pure = Second
  (<*>) (Second g) y = fmap g y
  (<*>) (First f) _ = First f

instance Monad (Sum a) where
  return = pure
  (>>=) (First x) _ = First x
  (>>=) (Second y) g = g y

mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g a = g a >>= f

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) f g a = f a >>= g
