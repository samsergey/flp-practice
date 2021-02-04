{-# LANGUAGE TupleSections, BangPatterns #-}
module Main where

import Data.List
import Data.Monoid
import GHC.Conc
import Data.List
import Data.Maybe

toBase :: Int -> Int -> [Int]
-- toBase b 0 = [0]
-- toBase b n = go [] n
--   where
--     go res 0 = res
--     go res n = go (mod n b : res) (div n b)

toBase b n = reverse
           $ map (`mod` b)
           $ takeWhile (> 0)
           $ iterate (`div` b) n

pascalStep r = zipWith (+) (0:r) (r++[0])

pascals = iterate pascalStep [1]

binomial n k = pascals !! n !! k

bernoulli n k = (tail . scanl (+) 0 <$> pascals) !! n !! k

binomial' n k = product [n - k + 1..n] `div` product [1..k]

floyd = go 1 [1..]
  where go i xs = let (a, b) = splitAt i xs
                  in a : go (i+1) b


floyd' = unfoldr (\(i,xs) -> Just $ (i+1,) <$> splitAt i xs) (1,[1..])

floyd'' :: Int -> [[Int]]
floyd'' x = [x] : ((x:) <$> floyd'' (x+1))

type RHS = Double -> Double -> Double
type Pt = (Double, Double)
type ODESolve = Double -> RHS -> Pt -> Pt

euler :: ODESolve
euler h f (x0, y0) = (x0 + h, y0 + h * f x0 y0)

euler' :: ODESolve
euler' h f (x0, y0) = case y' of
                      Nothing -> euler h f (x0, y0)
                      Just y' -> (x, y')
  where x = x0 + h
        y' = findRoot (\y -> h*(f x y) - (y-y0)) y0

rk2' :: ODESolve
rk2' h f (x0,y0) = case y' of
                      Nothing -> euler h f (x0, y0)
                      Just y' -> (x, y')
  where x = x0 + h
        y' = findRoot (\y -> y0 + h/2*(f x0 y0 + f x y)-y) y0

solveODE method f h = iterate (method h f) 



-- adaptive :: ODESolve -> ODESolve
-- adaptive method h f (x,y) = fromMaybe (method h f (x,y))
--                             $ fixedPointBy snd
--                             $ take 10
--                             $ ((\h -> method h f (x,y))
--                             <$> (iterate (/ 2) h))

findRoot' f df = fixedPoint
                 . take 150
                 . iterate (\x -> x - f x / df x)

findRoot f = findRoot' f (diff f)

fixedPoint :: [Double] -> Maybe Double
fixedPoint xs = snd <$>
                (find (\(x1,x2) -> abs(x2-x1) <= 1e-12)
                 $ zip xs (tail xs))

diff f x = fromMaybe (df 1e-8)
           $ fixedPoint
           $ take 20
           $ df <$> iterate (/ 2) 1e-3
  where df dx = (f (x + dx) - f(x - dx))/(2*dx)

sumsq 0 = 0
sumsq n = n^2 + sumsq (n-1)

sumsq' n = iter 0 1
  where iter s i = if i > n
                   then s
                   else iter (s + i^2) (i + 1)

fib 1 = 0
fib 2 = 1
fib n = fib (n-1) + fib (n-2)


fib' n = go 0 1 n
  where go a b 1 = a
        go a b 2 = b
        go a b i = go b (a+b) (i-1) 

pfib 1 = 0
pfib 2 = 1
pfib n = x `par` y `pseq` (x + y)
  where x = pfib (n-1)
        y = pfib (n-2)

main = print $ pfib 40 
