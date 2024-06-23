{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

import Data.Tuple (swap)
import Control.Monad.Trans.Except

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) =
    MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))

  (MaybeT mab) <*> (MaybeT ma) = MaybeT $ fmap (<*>) mab <*> ma

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT ma) >>= f =
    MaybeT $
      ma
        >>= ( \case
                Nothing -> return Nothing
                Just y -> runMaybeT (f y)
            )

newtype EitherT m a b
  = EitherT {runEitherT :: m (Either a b)}

instance (Functor m) => Functor (EitherT m a) where
  fmap f (EitherT ema) =
    EitherT $ (fmap . fmap) f ema

instance (Applicative m) => Applicative (EitherT m a) where
  pure x = EitherT $ (pure . pure) x
  (EitherT f) <*> (EitherT x) = EitherT $ fmap (<*>) f <*> x

instance (Monad m) => Monad (EitherT m a) where
  return = pure
  (EitherT mx) >>= f = EitherT $ do
    x <- mx
    case x of
      Left a -> return $ Left a
      Right b -> runEitherT (f b)

swapEither :: Either a b -> Either b a
swapEither (Left x) = Right x
swapEither (Right y) = Left y

swapEitherT :: (Functor m) => EitherT m a b -> EitherT m b a
swapEitherT (EitherT ema) = EitherT $ fmap swapEither ema

eitherT ::
  (Monad m) =>
  (a -> m c) ->
  (b -> m c) ->
  EitherT m a b ->
  m c
eitherT f g (EitherT mab) = mab >>= either f g

newtype ReaderT r m a
  = ReaderT {runReaderT :: r -> m a}

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) =
    ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure a = ReaderT (pure (pure a))
  (ReaderT fmab) <*> (ReaderT rma) =
    ReaderT $ (<*>) <$> fmab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  (>>=) ::
    ReaderT r m a ->
    (a -> ReaderT r m b) ->
    ReaderT r m b
  (ReaderT rma) >>= f =
    ReaderT $ \r -> do
      a <- rma r
      runReaderT (f a) r

newtype StateT s m a
  = StateT {runStateT :: s -> m (a, s)}

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT g) = StateT $ fmap (first f) . g

instance (Monad m) => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)

  (StateT mf) <*> (StateT mx) = StateT $ \s -> do
    (f, s') <- mf s
    (x, s'') <- mx s'
    return (f x, s'')

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT ma) >>= f = StateT $ \s -> do
    (a, s') <- ma s
    runStateT (f a) s'


