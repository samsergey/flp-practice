{-# language TypeSynonymInstances,FlexibleInstances #-}
module Lab2 where

import Prelude hiding ((&&), (||))
import Lab1 (mean)
import Data.List

data Electronics = R Double
                 | Key Bool
  deriving Show

data Circuit a = Elem a 
               | Par (Circuit a) (Circuit a)
               | Seq (Circuit a) (Circuit a) deriving Show

runCircuit :: DeMorgan a => (t -> a) -> Circuit t -> a
runCircuit el = go
  where
    go c = case c of
      Elem x -> el x 
      Seq c1 c2 -> go c1 && go c2
      Par c1 c2 -> go c1 || go c2


resistance :: Circuit Electronics -> Resistance Double
resistance = runCircuit el
  where el (R r) = Value r
        el (Key k) = if k then Short else Break

------------------------------------------------------------

resistanceToBool :: Resistance a -> Bool
resistanceToBool Short = True
resistanceToBool _ = False

boolToResistance :: Bool -> Resistance a
boolToResistance b = if b then Short else Break

resistanceToNum :: Fractional a => Resistance a -> a
resistanceToNum Short = 0
resistanceToNum Break = 1/0
resistanceToNum (Value x) = x

------------------------------------------------------------

data Resistance a = Short | Value a | Break
  deriving (Show, Eq, Ord)

class DeMorgan a where
  {-# MINIMAL inv, ((&&) | (||)) #-}
  one :: a
  zero :: a
  inv :: a -> a

  (&&) :: a -> a -> a
  a && b = inv (inv a || inv b)  

  (||) :: a -> a -> a
  a || b = inv (inv a && inv b)
  one = inv zero
  zero = inv one  

instance DeMorgan Bool where
  zero = False
  inv = not
  True && x = x
  False && _ = False

instance Fractional a => DeMorgan (Resistance a) where
  zero = Break
  inv Short = Break
  inv Break = Short
  inv (Value r) = Value (1/r)
  
  Break && _ = Break
  Short && r = r
  Value a && Value b = Value $ a + b
  a && b = b && a

instance DeMorgan (Circuit a) where
  inv (Elem x) = Elem x
  inv (Par c1 c2) = Seq (inv c1) (inv c2)
  inv (Seq c1 c2) = Par (inv c1) (inv c2)
  
  (&&) = Seq
  (||) = Par

instance DeMorgan String where
  zero = ""
  inv = id
  s1 && s2 = "(" ++ s1 ++ "," ++ s2 ++ ")"
  s1 || s2 = "(" ++ s1 ++ "+" ++ s2 ++ ")"

newtype Fuzzy = Fuzzy Double deriving Show

instance DeMorgan Fuzzy where
  zero = Fuzzy 0
  inv (Fuzzy x) = Fuzzy $ 1 - x
  Fuzzy v1 && Fuzzy v2 = Fuzzy (v1 * v2)

newtype Zadeh = Zadeh Double deriving Show

instance DeMorgan Zadeh where
  zero = Zadeh 0
  inv (Zadeh x) = Zadeh $ 1 - x
  Zadeh v1 && Zadeh v2 = Zadeh (v1 `min` v2)

xor :: DeMorgan a => a -> a -> a
xor a b = a || b && inv (a && b)

instance Num Zadeh where
  fromInteger 0 = Zadeh 0
  fromInteger _ = Zadeh 1

instance Fractional Zadeh where
  fromRational x = Zadeh (fromRational x)


instance Num Fuzzy where
  fromInteger 0 = Fuzzy 0
  fromInteger _ = Fuzzy 1

instance Fractional Fuzzy where
  fromRational x = Fuzzy (fromRational x)
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
                  Just c' -> Just c'
  where c = (a + b) / 2
                       
data Tree a = Node a (Tree a) (Tree a)
             deriving Show

tree :: (t -> (t,t)) -> t -> Tree t
tree f x = let (a, b) = f x
           in Node x (tree f a) (tree f b)

path :: (a -> Bool) -> Tree a -> [a]
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

floyd :: [[Int]]
floyd = (\i -> [arsum i + 1 .. arsum (i+1)]) <$> [1..]
  where arsum n = n*(n-1) `div` 2

------------------------------------------------------------

-- a 1 = Value 56
-- a 2 = Value 17
-- b 1 = Value 48
-- b 2 = Value 12
-- c 1 = Value 52
-- c 2 = Value 18
