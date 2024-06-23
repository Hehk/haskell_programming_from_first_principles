{-# LANGUAGE InstanceSigs #-}

newtype Compose f g a = Compose
  { getCompose :: f (g a)
  }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure x = Compose $ pure . pure $ x

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ fmap (<*>) f <*> a

-- Impossible
-- instance (Monad f, Monad g) => Monad (Compose f g) where
--   return = pure
--   (>>=) ::
--     Compose f g a ->
--     (a -> Compose f g b) ->
--     Compose f g b
--   (>>=) = undefined

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose a) = (foldMap . foldMap) f a

-- Plain old Identity. 'a' can be something with -- more structure, but it's not required and
-- Identity won't know anything about it.
newtype Identity a
  = Identity {runIdentity :: a}
  deriving (Eq, Show)

-- The identity monad transformer, serving only to
-- to specify that additional structure should exist.
newtype IdentityT f a
  = IdentityT {runIdentityT :: f a}
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT fab) <*> (IdentityT fa) =
    IdentityT (fab <*> fa)

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f =
    IdentityT $ ma >>= runIdentityT . f
