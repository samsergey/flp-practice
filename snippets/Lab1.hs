module Lab1 where

import Test.SmallCheck

circArea r = pi*r**2g

mean a b = (a + b)/2

fizzBuzz :: Int -> String
fizzBuzz n | n `mod` 3 == 0 = "Fizz"
           | n `mod` 5 == 0 = "Buzz"
           | n `mod` 15 == 0 = "FizzBuzz"
           | otherwise = show n
