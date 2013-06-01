--Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
--If d(a) = b and d(b) = a, where a  b, then a and b are an amicable pair and each of a and b are called amicable numbers.
--For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
--Evaluate the sum of all the amicable numbers under 10000.

-- To achieve greater speed, this should be compiled using `ghc -O prob21.hs`
-- and run from the command line using `prob21`.
import Data.List (group)

main :: IO()
main = print prob21

prob21 = sum [fst a + b | a <- sums, b <- [fst a..10000], snd a > fst a, amicable (fst a) b]
  where sums = map (\n -> (n, sumOfDivisors n)) [1..10000]

amicable i j
  | sumOfDivisors i == j && sumOfDivisors j == i    = True
  | otherwise                                       = False

sumOfDivisors n = (product $ map sum $ ds) - n
  where ds = map (\f -> [1] ++ [(head f) ^ e | e <- [1..length f]]) pfs
        pfs = group $ primeFactors n

-- From prob12.has:
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