customTake :: Integer -> [a] -> [a]
customTake 0 xs = []
customTake n [] = []
customTake n (x:xs) = x : customTake (n - 1) xs
