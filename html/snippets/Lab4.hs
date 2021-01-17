{-# LANGUAGE BangPatterns #-}
module Lab4 where

import Data.Char
import Data.List

md = 100001
  
hashStep :: Int -> Char -> Int
hashStep s ch = (s * 256 + ord ch) `mod` md

hash :: String -> Int
hash = foldl hashStep 0 

hashAdd :: Int -> Char -> Char -> Int -> Int
hashAdd n a b s = hashStep (s - ord a*pow 256 n md) b

pow :: Int -> Int -> Int -> Int
pow a b m = foldl (\r x -> (r*x) `mod` m) a $ replicate (b-1) a

--findSub :: String -> String -> [Int]
--findSub [sub] s = elemIndices sub s
findSub sub s   = map (\(i,_,_) -> i) $
                  filter test $
                  zip3 [0..] (tails s) $
                  scanl' hashAdd (hash prefix) $
                  zip s s'
  where
    n = length sub
    (prefix, s') = splitAt n s
    hashAdd s (!a, !b) = hashStep (s - ord a*shift) b
    shift = pow 256 (n-1) md
    hs = hash sub
    test (_, s', h) = h == hs && take n s' == sub

nums = concat $ show <$> [1..]

subs :: String -> String -> [Int]
subs sub s = elemIndices sub $ take (length sub) <$> tails s
