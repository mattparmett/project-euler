--The sequence of triangle numbers is generated by adding the natural numbers. So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:

--1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

--Let us list the factors of the first seven triangle numbers:

-- 1: 1
-- 3: 1,3
-- 6: 1,2,3,6
--10: 1,2,5,10
--15: 1,3,5,15
--21: 1,3,7,21
--28: 1,2,4,7,14,28
--We can see that 28 is the first triangle number to have over five divisors.

--What is the value of the first triangle number to have over five hundred divisors?

import Data.List (group)

prob12 = head $ filter ((> 500) . numFactors) triangleNums
  where triangleNums = scanl1 (+) [1..]
        -- http://www.gmathacks.com/gmat-math/number-of-factors-of-a-large-integer.html
        numFactors n = product $ map ((+ 1) . length) $ group $ primeFactors n

-- From prob3.hs:
primes = 2 : filter ((== 1) . length . primeFactors) [3,5..]

primeFactors :: Integer -> [Integer]
primeFactors n = factor n primes

factor :: Integer -> [Integer] -> [Integer]
factor n candidates@(c:cs)
  | c*c > n           = [n]
  | n `mod` c == 0    = c : factor (n `div` c) candidates
  | otherwise         = factor n cs