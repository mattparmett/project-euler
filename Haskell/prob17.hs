--If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
--If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
--NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
import Data.Char (isAlpha, digitToInt)

prob17 = sum $ map (length . filter isAlpha . numToWords) [1..1000]

numToWords n
  | n < 1       = []
  | n < 20      = small !! (n-1)
  | n < 100     = (tens !! (firstDigit n - 2)) ++ numToWords (n - (10 * firstDigit n))
  | n < 1000    = (small !! (firstDigit n - 1))
                  ++ " hundred"
                  ++ if n `mod` 100 /= 0
                     then " and " ++ numToWords (n - (100 * firstDigit n))
                     else ""
  | n == 1000   = "one thousand"
  where small= ["one", "two", "three", "four", "five", "six",
                "seven", "eight", "nine", "ten", "eleven",
                "twelve", "thirteen", "fourteen", "fifteen",
                "sixteen", "seventeen", "eighteen", "nineteen"]
        tens = ["twenty", "thirty", "forty", "fifty", "sixty",
                "seventy", "eighty", "ninety"]
        firstDigit n = (digitToInt $ head $ show n) :: Int