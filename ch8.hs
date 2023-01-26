applyTimes :: (Num a) => a -> (a -> a) -> Integer -> a
applyTimes a _ 0 = a
applyTimes a fun times = applyTimes (fun a) fun (times - 1)

sumToN :: Integer -> Integer 
sumToN 0 = 0
sumToN x = x + sumToN (x - 1)

recurSummation :: (Integral a) => a -> a -> a
recurSummation 0 _ = 0
recurSummation _ 0 = 0
recurSummation a b = a + recurSummation a (b - 1)