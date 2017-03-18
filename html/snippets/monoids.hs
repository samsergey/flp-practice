import Data.Monoid
import Control.Applicative

fizzbuzz = max . show <*>
           (when (divBy 3) "Fizz" <> when (divBy 5) "Buzz")

divBy m x = x `mod` m == 0

when p m x = if p x then m else mempty
