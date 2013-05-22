--The prime factors of 13195 are 5, 7, 13 and 29.
--What is the largest prime factor of the number 600851475143 ?

prob3 = last (primeFactors 600851475143)

-- primes is an infinite list of prime numbers,
-- numbers which have one prime factor
-- Kept at the global level so we only calculate once.
primes = 2 : filter ((== 1) . length . primeFactors) [3,5..]

-- primeFactors prime factorizes the given integer n
-- and returns a list of all it's prime factors.
primeFactors :: Integer -> [Integer]
primeFactors n = factor n primes

factor :: Integer -> [Integer] -> [Integer]
factor n candidates@(c:cs)
  | c*c > n           = [n]
  | n `mod` c == 0    = c : factor (n `div` c) candidates
  | otherwise         = factor n cs