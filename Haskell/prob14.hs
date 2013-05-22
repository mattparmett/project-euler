----The following iterative sequence is defined for the set of positive integers:
----n  n/2 (n is even)
----n  3n + 1 (n is odd)
----Using the rule above and starting with 13, we generate the following sequence:
----13  40  20  10  5  16  8  4  2  1
----It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
----Which starting number, under one million, produces the longest chain?
----NOTE: Once the chain starts the terms are allowed to go above one million.

import Data.List (sortBy)
import Data.Function (on)

collatzChain :: Integer -> [Integer]
collatzChain 1 = [1]
collatzChain n
  | odd n   = n : collatzChain (incrOdd n)
  | even n  = n : collatzChain (incrEven n)
  where incrEven n = n `div` 2
        incrOdd n = 3 * n + 1

-- Ok, so we have to be careful to avoid stack overflows here.
-- (The working integers are large and I am, after all, on 32 bit Windows.)
-- I'll make some optimizations to the initial solution set:
    -- Let's use only odd numbers; they get multiplied by 3 first,
      -- so they are the most likely candidates for longest chain.
    -- Let's traverse the solution set backwards, as this end will
      -- be more likely to contain the longest chain.
-- Finally, so the program doesn't take forever to run, let's throw 
-- the logic into main and compile it into an exe (ghc -O prob14.hs).
-- Run with `prob14` at the command prompt.
main :: IO()
main = do
    let chains = map (\n -> (n, length $ collatzChain n)) [999999, 999997..3]
    print . fst $ last $ sortBy (compare `on` snd) chains