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

------------------------------------------------------------

data M a = D a | M [[a]]
  deriving Show

toList (M a) = a
toList (D a) = iterate (0 :) (a : repeat 0)

dot a b = sum $ zipWith (*) a b                 

tr (D a) = D a
tr (M a) = M $ transpose a

instance Num a => Semigroup (M a) where
  D a <> D b = D $ a * b
  D a <> M b = M $ map (map (* a)) b
  M b <> D a = M $ map (map (* a)) b
  M a <> M b = M $ [ [ dot x y | y <- transpose b ] | x <- a ]

instance Num a => Monoid (M a) where
    mempty = D 1

D a <+> D b = D (a + b)
a <+> b = M $ zipWith (zipWith (+)) (toList a) (toList b)
             
jumps = [ (3, 22)
        , (5, 8)
        , (11, 26)
        , (17, 4)
        , (19, 7)
        , (20, 29)
        , (21, 9)
        , (27, 1)]

jump i j = case lookup i jumps of
             Just j' | j == j' -> 1.0
             Nothing | i == j -> 1.0
             _ -> 0.0

move i j
  | i == 30 = 0 -- && j == 30 = 1
  | j == 30 && 30 - i <= 6 = 1 - 1/6*(30 - i - 1)
  | i < j && j <= i + 6 = 1/6
  | otherwise = 0

matrix f rng = M $ [ [f i j | i <- rng ] | j <- rng ]

jumpsM = matrix jump [1..30]
movesM = matrix move [1..30]
gameM = jumpsM <> movesM

powers x = mempty : zipWith (<>) (powers x) (repeat x)

times 0 _ = mempty
times 1 a = a
times 2 a = a <> a
times n a
  | even n = (n `div` 2) `times` (a <> a)
  | odd n  = (n - 1) `times` a <> a

diffs lst = zipWith (-) (tail lst) lst

mean lst = dot lst [1..]
--median pmf = 
