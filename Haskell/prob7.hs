--By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
--What is the 10 001st prime number?

prob7 = primes !! 10000
  -- From prob3.hs:
  where primes = 2 : filter ((== 1) . length . primeFactors) [3,5..]
        primeFactors n = factor n primes
        factor n (p:ps)
          | p*p > n           = [n]
          | n `mod` p == 0    = p : factor (n `div` p) (p:ps)
          | otherwise         = factor n ps