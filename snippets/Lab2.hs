module Lab2 where

import Lab1 (mean)
import Data.List

  
data Circuit = R Double
               | Par Circuit Circuit
               | Seq Circuit Circuit deriving Show

resistance :: Circuit -> Double
resistance c = case c of
  R r -> r
  Seq c1 c2 -> 1/(1/resistance c1 + 1/resistance c2)
  Par c1 c2 -> resistance c1 + resistance c2



bisection :: Eq a
          => (Double -> a)
          -> Double -> Double
          -> Maybe Double 
bisection test a b
  -- тестовая функция не меняется на границах
  | test a == test b = Nothing
  -- достигнута абсолютная или относительная погрешность
  | abs (b - a) < 1e-11 = Just c
  -- шаг бисекции
  | otherwise = case bisection test a c of
                  Nothing -> bisection test c b
                  Just c -> Just c
  where c = (a + b) / 2
                       
data Tree a = Node a (Tree a) (Tree a)
             deriving Show

tree f x = let (a, b) = f x
           in Node x (tree f a) (tree f b)

path p (Node a t1 t2) =
  if p a then [] else [a] ++ path p t1 ++ path p t2 

bisection'
  :: Eq a2 =>
     (Double -> a2) -> (Double, Double) -> Maybe Double
bisection' p =
  (uncurry mean <$>) .
  find (\(a, b) -> abs (b - a) < 1e-11) .
  path (\(a, b) -> p a == p b) .
  tree (\(a, b) -> let c = mean a b in ((a,c),(c,b)))

listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

floyd = (\i -> [tr i + 1 .. tr (i+1)]) <$> [1..]
  where tr n = n*(n-1) `div` 2
