--n! means n  (n  1)  ...  3  2  1

--For example, 10! = 10  9  ...  3  2  1 = 3628800,
--and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

--Find the sum of the digits in the number 100!
import Data.Char (digitToInt)

prob20 = sum $ map digitToInt $ show $ factorial 100
  where factorial 1 = 1
        factorial n = n * factorial (n-1)