{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Lab3 where

import Data.List (find, unfoldr)
import Data.Maybe (fromMaybe)
import Data.Monoid ()
import GHC.Conc (par, pseq)
import Lab1 (mean, gauss)
import Lab2 (bisection)

toBase' :: Int -> Int -> [Int]
toBase' b n
  | b <= 1 = error ""
  | n == 0 = [0]
  | otherwise = go [] n
  where
    go res 0 = res
    go res n = go (mod n b : res) (div n b)

toBase :: Int -> Int -> [Int]
toBase b n
  | b <= 1 = error ""
  | n == 0 = [0]
  | otherwise = reverse $ map (`mod` b) $ takeWhile (> 0) $ iterate (`div` b) n

pascalStep :: Num a => [a] -> [a]
pascalStep r = zipWith (+) (0 : r) (r ++ [0])

pascals :: [[Integer]]
pascals = iterate pascalStep [1]

binomial :: Int -> Int -> Integer
binomial n k = pascals !! n !! k

bernoulli :: Int -> Int -> Integer
bernoulli n k = (tail . scanl (+) 0 <$> pascals) !! n !! k

binomial' :: Integral a => a -> a -> a
binomial' n k = product [n - k + 1 .. n] `div` product [1 .. k]


floyd :: [[Integer]]
floyd = [ [arsum i + 1 .. arsum (i + 1)] | i <- [1..] ]
  where arsum n = (n * (n - 1)) `div` 2

floyd' :: [[Integer]]
floyd' = unfoldr (\(i, xs) -> Just $ (i + 1, ) <$> splitAt i xs) (1, [1 ..])

floyd'' :: Int -> [[Int]]
floyd'' x = [x] : ((x :) <$> floyd'' (x + 1))

type RHS = Double -> Double -> Double
type Pt = (Double, Double)
type ODESolve = Double -> RHS -> Pt -> Pt

euler :: ODESolve
euler h f (x0, y0) = (x0 + h, y0 + h * f x0 y0)

euler' :: ODESolve
euler' h f (x0, y0) =
  case y' of
    Nothing -> euler h f (x0, y0)
    Just y' -> (x, y')
  where
    x = x0 + h
    y' = findRoot (\y -> h * f x y - (y - y0)) y0

rk2' :: ODESolve
rk2' h f (x0, y0) =
  case y' of
    Nothing -> euler h f (x0, y0)
    Just y'' -> (x, y'')
  where
    x = x0 + h
    y' = findRoot (\y -> y0 + h / 2 * (f x0 y0 + f x y) - y) y0

solveODE :: (t1 -> t2 -> a -> a) -> t2 -> t1 -> a -> [a]
solveODE method f h = iterate (method h f)

-- adaptive :: ODESolve -> ODESolve
-- adaptive method h f (x,y) = fromMaybe (method h f (x,y))
--                             $ fixedPointBy snd
--                             $ take 10
--                             $ ((\h -> method h f (x,y))
--                             <$> (iterate (/ 2) h))

findRoot' :: (Double -> Double) -> (Double -> Double) -> Double -> Maybe Double
findRoot' f df = fixedPoint . take 150 . iterate (\x -> x - f x / df x)

findRoot :: (Double -> Double) -> Double -> Maybe Double
findRoot f = findRoot' f (diff f)

fixedPoint :: [Double] -> Maybe Double
fixedPoint xs =
  snd <$> find (\(x1, x2) -> abs (x2 - x1) <= 1e-12) (zip xs (tail xs))

diff :: (Double -> Double) -> Double -> Double
diff f x =
  fromMaybe (df 1e-8) $ fixedPoint $ take 20 $ df <$> iterate (/ 2) 1e-3
  where
    df dx = (f (x + dx) - f (x - dx)) / (2 * dx)

sumsq :: (Num p, Eq p) => p -> p
sumsq 0 = 0
sumsq n = n ^ 2 + sumsq (n - 1)

sumsq' :: (Num t, Ord t) => t -> t
sumsq' n = iter 0 1
  where
    iter s i =
      if i > n
        then s
        else iter (s + i ^ 2) (i + 1)

fib :: (Eq a, Num a, Num p) => a -> p
fib 1 = 0
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

fib' :: Integer -> Integer
fib' = go 0 1
  where
    go a b 1 = a
    go a b 2 = b
    go a b i = go b (a + b) (i - 1)

pfib :: (Eq a, Num p, Num a) => a -> p
pfib 1 = 0
pfib 2 = 1
pfib n = x `par` y `pseq` (x + y)
  where
    x = pfib (n - 1)
    y = pfib (n - 2)

main :: IO ()
main = print $ pfib 40

floyd''' :: [[Integer]]
floyd''' = map (\i -> [arsum i + 1 .. arsum (i + 1)]) [1 ..]
  where
    arsum n = (n * (n - 1)) `div` 2

data Tree a = Node a (Tree a) (Tree a)
  deriving (Show, Foldable)

--instance Foldable Tree where
--  foldMap f (Node x t1 t2) = f x <> foldMap f t1 <> foldMap f t2

tree :: (a -> (a, a)) -> a -> Tree a
tree f x = let (a, b) = f x
           in Node x (tree f a) (tree f b)

path :: (a -> Bool) -> Tree a -> [a]
path p (Node a t1 t2)
  | p a = [a] ++ path p t1 ++ path p t2
  | otherwise = []

path' p = foldMap $ \a -> if not(p a) then [a] else mempty

bisection' ::
  Eq a => (Double -> a)
       -> (Double, Double)
       -> Maybe Double
bisection' p =
  fmap (\(a, b) -> mean a b) .
  find (\(a, b) -> abs (b - a) < 1e-11) .
  path' (\(a, b) -> p a /= p b) .
  tree (\(a, b) -> let c = mean a b in ((a,c),(c,b)))

integrate :: (Double -> Double) -> [Double] -> Double
integrate f mesh = sum $ zipWith (gauss f) mesh (tail mesh)
