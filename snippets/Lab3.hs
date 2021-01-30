{-# LANGUAGE TupleSections, BangPatterns #-}
module Lab3 where

import Data.List
import Data.Monoid

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

sumsq 0 = 0
sumsq n = n^2 + sumsq (n-1)

sumsq' n = iter 0 1
  where iter s i = if i > n
                   then s
                   else iter (s + i^2) (i + 1)
