module Chapter14Scratch () where

import Test.QuickCheck

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = do
    Only <$> arbitrary

instance (Semigroup a) => Semigroup (Optional a) where
  (Only x) <> (Only y) = Only (x <> y)
  _ <> (Only y) = Only y
  (Only x) <> _ = Only x
  _ <> _ = Nada

instance (Monoid a) => Monoid (Optional a) where
  mempty = Nada

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype First' a = First' {getFirst' :: Optional a} deriving (Eq, Show)

instance Semigroup (First' a) where
  (First' Nada) <> x = x
  x <> _ = x

instance Monoid (First' a) where
  mempty = First' Nada
  mappend = (<>)

firstMappend :: (Monoid a) => First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
  First' String ->
  First' String ->
  First' String ->
  Bool

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = do
    First' <$> arbitrary

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: First' String -> Bool)
  quickCheck (monoidRightIdentity :: First' String -> Bool)
