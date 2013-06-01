--A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
--A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.
--As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.
--Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

-- Not solved yet...

import Data.List (group, nub)

abundants = filter abundant [1..]
  where abundant n = sum $ properDivisors n > n

properDivisors :: Int -> [Int]
properDivisors n = nub $ concat $ factors
  -- From prob21.hs:
  where factors = map (\f -> [1] ++ [(head f) ^ e | e <- [1..length f]]) pfs
        pfs = group $ primeFactors n

-- From prob3.hs:
primes = 2 : filter ((== 1) . length . primeFactors) [3,5..]

primeFactors :: Integer -> [Integer]
primeFactors n = factor n primes

factor :: Integer -> [Integer] -> [Integer]
factor n candidates@(c:cs)
  | c*c > n           = [n]
  | n `mod` c == 0    = c : factor (n `div` c) candidates
  | otherwise         = factor n cs