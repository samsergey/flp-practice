{-# LANGUAGE TupleSections, BangPatterns #-}
module Main where

import Data.List
import Data.Monoid
import GHC.Conc

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

euler f h (x0, y0) = (x0 + h, y0 + h * f x0 y0)

euler' f h (x0, y0) = (x, y)
  where x = x0 + h
        y = findroot (\y -> f x y - (y-y0)/h) (f x) y0

findroot f df = fixPoint 1e-8 (\x -> x - f x / df x)

fixPoint dx f x = snd $ dropWhile (\(x1,x2) -> abs(x2-x1) < 1e-8) $ iterate f x

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
