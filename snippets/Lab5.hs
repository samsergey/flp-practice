module Lab5 where

import Data.Monoid
import Data.List

when m p x = if p x then m else mempty

fizzBuzz n =
  show n `max`
  ("Fizz" `when` divisibleBy 3 <>
   "Buzz" `when` divisibleBy 5) n
  where divisibleBy n x = x `mod` n == 0

collatz :: Integer -> Integer
collatz n | even n = n `div` 2
          | odd n  = 3*n+1

