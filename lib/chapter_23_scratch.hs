{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie 1 = DieOne
intToDie 2 = DieTwo
intToDie 3 = DieThree
intToDie 4 = DieFour
intToDie 5 = DieFive
intToDie 6 = DieSix
intToDie x = error $ "intToDie got non 1-6 integer: " ++ show x

rollDie :: StdGen -> (Die, StdGen)
rollDie seed =
  let (n, newSeed) = randomR (1, 6) seed
   in (intToDie n, newSeed)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes :: State StdGen (Die, Die, Die)
rollDieThreeTimes =
  liftA3 (,,) rollDie' rollDie' rollDie'

rollsToGetN :: Int -> StdGen -> (Int, [Die])
rollsToGetN n = go (0, [])
  where
    go :: (Int, [Die]) -> StdGen -> (Int, [Die])
    go (sum, dice) gen
      | sum >= n = (sum, dice)
      | otherwise =
          let (die, nextGen) = randomR (1, 6) gen
           in go ((sum + die), ((intToDie die) : dice)) nextGen

newtype Moi s a
  = Moi {runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) =
    Moi $ \s ->
      let (x, s') = g s
       in (f x, s')

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ (,) a

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) =
    Moi $ \s ->
      let (fn, s') = f s
          (x, s'') = g s'
       in (fn x, s'')

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s ->
    let (a, ns) = f s
        Moi g' = g a
     in g' ns
