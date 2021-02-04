module Lab5 where

import Data.Monoid

when m p x = if p x then m else mempty

fizzBuzz n =
  show n `max`
  ("Fizz" `when` divisibleBy 3 <>
   "Buzz" `when` divisibleBy 5) n
  where divisibleBy n x = x `mod` n == 0



