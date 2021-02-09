module Lab5 where

import Data.Monoid
import Data.List

when :: Monoid m => (a -> m) -> (a -> Bool) -> a -> m
when m p x = if p x then m x else mempty

fizzBuzz n =
  show n `max`
  (const "Fizz" `when` divisibleBy 3 <>
   const "Buzz" `when` divisibleBy 5) n
  where divisibleBy n x = x `mod` n == 0

collatz :: Integer -> Integer
collatz n | even n = n `div` 2
          | odd n  = 3*n+1

path' :: Foldable t => (a -> Bool) -> t a -> [a]
path' p = foldMap $ pure `when` p

facts = 1 : map (*) (tail facts)