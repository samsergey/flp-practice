{-# language TypeSynonymInstances,FlexibleInstances #-}
module Lab2 where

import Lab1 (mean)
import Data.List
import Data.Ord

data Electronics = R Double
                 | Key Bool
  deriving Show

data Circuit a = Elem a 
               | Par (Circuit a) (Circuit a)
               | Seq (Circuit a) (Circuit a) deriving Show

runCircuit elem = go
  where
    go c = case c of
      Elem x -> elem x 
      Seq c1 c2 -> go c1 <&&> go c2
      Par c1 c2 -> go c1 <||> go c2


resistance :: Circuit Electronics -> Resistance Double
resistance = runCircuit elem
  where elem (R r) = Value r
        elem (Key k) = if k then Short else Break

------------------------------------------------------------

resistanceToBool :: Resistance a -> Bool
resistanceToBool Short = True
resistanceToBool _ = False

boolToResistance :: Bool -> Resistance a
boolToResistance b = if b then Short else Break

--resistanceToNum :: Num a => Resistance a -> a
resistanceToNum Short = 0
resistanceToNum Break = 1/0
resistanceToNum (Value x) = x

------------------------------------------------------------

data Resistance a = Short | Value a | Break
  deriving (Show, Eq, Ord)

class DeMorgan a where
  inv :: a -> a

  (<&&>) :: a -> a -> a
  a <&&> b = inv (inv a <||> inv b)  

  (<||>) :: a -> a -> a
  a <||> b = inv (inv a <&&> inv b)
    
instance Fractional a => DeMorgan (Resistance a) where
  inv Short = Break
  inv Break = Short
  inv (Value r) = Value (1/r)
  
  Break <&&> _ = Break
  Short <&&> r = r
  Value a <&&> Value b = Value $ a + b
  a <&&> b = b <&&> a

instance DeMorgan (Circuit a) where
  inv (Elem x) = Elem x
  inv (Par c1 c2) = Seq (inv c1) (inv c2)
  inv (Seq c1 c2) = Par (inv c1) (inv c2)
  
  (<&&>) = Seq
  (<||>) = Par

instance DeMorgan String where
  inv = id
  s1 <&&> s2 = "(" ++ s1 ++ "," ++ s2 ++ ")"
  s1 <||> s2 = "(" ++ s1 ++ "+" ++ s2 ++ ")"

------------------------------------------------------------

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


floyd = (\i -> [arsum i + 1 .. arsum (i+1)]) <$> [1..]
  where arsum n = n*(n-1) `div` 2

------------------------------------------------------------

a 1 = Value 56
a 2 = Value 17
b 1 = Value 48
b 2 = Value 12
c 1 = Value 52
c 2 = Value 18
