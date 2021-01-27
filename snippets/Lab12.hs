{-# LANGUAGE FlexibleInstances,FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# language MultiParamTypeClasses #-}
module Lab12 where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List

newtype Logic a = From { toList :: [a] }

instance Show a => Show (Logic a) where
  show (From l) = "Logic " <> case l of
                                [] -> "[]"
                                [x] -> show x
                                x:_ -> show x <> " ..."
headL :: Logic a -> Maybe a 
headL = listToMaybe . toList

takeL :: Int -> Logic a -> [a]
takeL n = take n . toList

interleave l1 l2 = case l1 of
  [] -> l2
  x:xs -> x : interleave l2 xs

instance Functor Logic where
  fmap f (From lst) = From $ fmap f lst

instance Applicative Logic where
  pure x = From [x]
  (<*>) = ap

instance Semigroup (Logic a) where
  From l1 <> From l2  = From $ l1 <> l2

instance Monoid (Logic a) where
  mempty = empty


instance Alternative Logic where
  empty = From []
  From l1 <|> From l2  = From $ interleave l1 l2

instance Monad Logic where
  From [] >>= _ = empty
  From (x:xs) >>= f = f x <|> (From xs >>= f)

instance MonadFail Logic where
  fail _ = empty

different :: Eq a => Int -> [a] -> Logic [a]
different 0 _ = pure []
different n lst = do x <- From lst
                     xs <- different (n-1) $ filter (/= x) lst
                     pure (x:xs)

samples :: Int -> [a] -> Logic [a]
samples 0 _ = pure []
samples n lst = do x <- From lst
                   xs <- samples (n-1) lst
                   pure (x:xs)

fact 0 x = x == 1
fact n m = m `mod` n == 0 && fact (n-1) (m `div` n)

 
prob1 = do [a,p,k] <- different 3 [1,2,3]
           guard $ and [ a /= 2
                       , p > a
                       , k /= 3 ]
           pure (a,p,k)


queens :: Int -> Logic [Int]
queens n = go n
    where
      go 0 = pure []
      go k = do qs <- go (k-1)
                q <- From [1..n]
                guard . not $ q `elem` qs
                guard . not $ sameDiag q qs
                pure (q:qs)
      sameDiag q = any (\(c, x) -> abs (q - x) == c) . zip [1..]

showQueens n = foldMap line
  where
    line i = replicate (i-1) '.'
             <> "Q"
             <> replicate (n-i+1) '.'
             <> "\n"



-- пилеты

class Search s m  where
  move :: s -> [([m], s)]
  isSolution :: ([m], s) -> Bool

  space :: s -> [([m], s)]
  space s = step <> expand step
    where
      step = move s
      expand ss = do (m, s) <- ss
                     (n, t) <- space s
                     return (m <> n, t)

  solutions :: s -> [([m], s)]
  solutions = filter isSolution . space

data Toy = Buzz | Hamm | Rex | Woody deriving (Eq,Ord,Show)
data Pos = L | R deriving (Eq,Show)
type Group = [Toy]
type BridgePos = (Pos,Group)
type Move = Either Toy Group

toys :: [Toy]
toys = [Buzz,Hamm,Rex,Woody]

time :: Toy -> Int
time Buzz = 5
time Woody = 10
time Rex = 20
time Hamm = 25

duration :: [Move] -> Int
duration = sum . map (either time (maximum.map time))

backw :: Group -> [([Move],BridgePos)]
backw xs = [([Left x],(L,sort (x:(toys \\ xs)))) | x <- xs]

forw :: Group -> [([Move],BridgePos)]
forw xs = [([Right [x,y]],(R,delete y ys)) |
           x <- xs,let ys=delete x xs, y <- ys, x<y]


instance Search BridgePos Move where
  move (L,l) = forw l
  move (R,l) = backw (toys \\ l)
  isSolution (ms,s) = s == (R,[]) && duration ms <= 60

