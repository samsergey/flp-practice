{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module Lab5 where

import Data.Semigroup
import Data.Monoid
import Data.List
import Text.Printf
import Data.Foldable
import Lab3 (Tree(..), tree, (+++))
import Lab4 

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

data M a = I | M [[a]]
  deriving Show

toListM (M a) = a
           
dot a b = sum $ zipWith (*) a b                 

instance Num a => Semigroup (M a) where
  I <> x = x
  x <> I = x
  M a <> M b = M $ [ [ dot x y | y <- transpose b ] | x <- a ]

instance Num a => Monoid (M a) where
    mempty = I

matrix f rng = M $ [ [f i j | i <- rng ] | j <- rng ]
             
powers x = mempty : zipWith (<>) (powers x) (repeat x)

diagonal m = zipWith (!!) (toListM m) [0..]
trace = sum . diagonal
           
times 0 _ = mempty
times 1 a = a
times 2 a = a <> a
times n a
  | even n = (n `div` 2) `times` (a <> a)
  | odd n  = a <> (n - 1) `times` a

diffs lst = zipWith (-) (tail lst) lst

mean lst = dot lst [1..]
--median pmf = 

market = M [ [0.9, 0.075, 0.025]
           , [0.15, 0.8, 0.05 ]
           , [0.25, 0.25, 0.5] ]

adj = M $ [ [0,1,0,0,0,1]
          , [0,0,1,0,0,0]
          , [0,0,0,1,0,0]
          , [0,0,0,0,1,0]
          , [1,1,0,0,0,0]
          , [0,0,1,1,0,0]]

rotate (x:xs) = xs ++ [x]
rotate' = reverse . rotate . reverse
rotations lst = take (length lst) $ iterate rotate' lst

tr = M . transpose . toListM
                
moves = tr . M . rotate . rotations $ replicate 6 (1/6) ++ replicate 6 0
        --  0 1 2 3 4 5 6 7 8 9 1011

jumps = tr $ M [[1,0,0,0,0,0,0,0,0,0,0,0] --0  
               ,[0,1,0,0,0,0,0,0,0,0,0,0] --1
               ,[0,0,1,0,0,0,0,0,0,0,0,0] --2
               ,[0,0,0,0,0,0,0,0,0,1,0,0] --3
               ,[0,0,0,0,1,0,0,0,0,0,0,0] --4
               ,[0,0,0,0,0,0,0,0,1,0,0,0] --5
               ,[0,0,0,0,0,0,1,0,0,0,0,0] --6
               ,[0,0,0,0,0,0,0,1,0,0,0,0] --7
               ,[0,0,0,0,0,0,0,0,1,0,0,0] --8
               ,[0,0,0,0,0,0,0,0,0,1,0,0] --9
               ,[0,0,0,0,0,0,0,1,0,0,0,0] --10
               ,[0,0,0,0,0,0,1,0,0,0,0,0]]--11

--gameM :: M Doubleg
gameM = jumps <> moves

setM i j x (M m) = let (a, b:c) = splitAt i m
                       (d, _:f) = splitAt j b
                   in M $ a <> ((d <> (x : f)) : c)

scale a (M m) = M $ map (map (a*)) m
                      
--             I      A     C      P     D
merch a c = M [ [ 0.98-2*a, 0.015+a, 0.005+a, 0,    0    ]   -- I
              , [ 0.09, 0.61-c,  0.10+c,  0.12, 0.08 ]   -- A
              , [ 0.07, 0.05,  0.25,  0.53, 0.10 ]   -- C
              , [ 0,    0.01,  0.05,  0.70, 0.24 ]   -- P
              , [ 0,    0.50,  0.20,  0.15, 0.15 ] ] -- D

fib n = let M [[_,x],[_,_]] = n `times` M [[1,1],[1,0]] in x

fibi n = go 0 1 n
  where go a b 0 = a
        go a b 1 = b
        go a b n = go b (a + b) (n - 1)

data RTree a = RTree a [RTree a]
  deriving Show

takeRTree 0 (RTree a _) = RTree a []
takeRTree n (RTree a ts) = RTree a (takeRTree (n-1) <$> ts)

instance Foldable RTree where
  foldMap f = foldMap f . bfs
    where
      bfs (RTree a brs) = [a] <> foldl (+++) [] (bfs <$> brs)

rTree f a = RTree a (rTree f <$> f a)

t = rTree (\x -> ['(':x, ')':x, '[':x, ']':x]) []
