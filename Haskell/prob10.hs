--The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--Find the sum of all the primes below two million.

prob10 = sum (takeWhile (< 2000000) primes)
  -- From prob3.hs:
  where primes = 2 : filter ((== 1) . length . primeFactors) [3,5..]
        primeFactors n = factor n primes
        factor n (p:ps)
          | p*p > n           = [n]
          | n `mod` p == 0    = p : factor (n `div` p) (p:ps)
          | otherwise         = factor n ps