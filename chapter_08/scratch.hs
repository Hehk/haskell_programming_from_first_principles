factorial :: Integer -> Integer
factorial 0 = 1
factorial n = factorial (n - 1) * n

incTimes :: Integer -> Integer -> Integer
incTimes n 0 = n
incTimes n m = incTimes (n + 1) (m - 1)
