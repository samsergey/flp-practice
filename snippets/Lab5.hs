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



data M a = I | M [[a]] deriving Show

instance Num a => Semigroup (M a) where
    I <> x = x
    x <> I = x
    M a <> M b = M $ [[dot x y | y <- transpose b ] | x <- a ]

instance Num a => Monoid (M a) where
    mempty = I
                 
dot a b = sum $ zipWith (*) a b                 

jumps = [ (3, 22)
        , (5, 8)
        , (11, 26)
        , (17, 4)
        , (19, 7)
        , (20, 29)
        , (21, 9)
        , (27, 1)
        , (30, 30) ]

isJump i j = case lookup i jumps of
              Nothing -> False
              Just j' -> j == j'

moves i j  | isJump i j = 1
           | j == 30 && 30 - i < 6 = 1 - (30 - i)/6
           | i < j && j <= i+6 = 1/6
           | otherwise = 0

game = [[moves j i | i <- [1..30] ] | j <- [1..30]]

--times :: (Num n, Integral i) => i -> n -> n
times 0 _ = mempty
times 1 a = a
times 2 a = a <> a
times n a
  | even n = (n `div` 2) `times` (a <> a)
  | odd n  = (n - 1) `times` a <> a
