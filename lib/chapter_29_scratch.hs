import Control.Concurrent
import Debug.Trace
import System.Random

myData :: IO (MVar Int)
myData = newEmptyMVar

run :: IO ()
run = do
  mv <- myData
  putMVar mv 0
  mv' <- myData
  zero <- takeMVar mv'
  print zero

blah :: IO String
blah = return "blah"

blah' = trace "outer trace" blah

woot :: IO String
woot = return (trace "inner trace" "woot")

run' :: IO ()
run' = do
  b <- blah'
  putStrLn b
  putStrLn b
  w <- woot
  putStrLn w
  putStrLn w

