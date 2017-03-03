{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor,GeneralizedNewtypeDeriving #-}
import Data.Bool

------------------------------------------------------------

class Semiring a where
  inv :: a -> a
  (<&&>) :: a -> a -> a
  (<||>) :: a -> a -> a
  
  a <||> b = inv (inv a <&&> inv b)
  a <&&> b = inv (inv a <||> inv b)

instance Semiring Bool where
  (<&&>) = (&&)
  inv = not

------------------------------------------------------------

newtype Frac a = Frac {getFrac :: a}
  deriving (Show,Eq,Ord,Num,Functor,Fractional)

instance Fractional a => Semiring (Frac a) where
  inv (Frac r) = Frac (1/r)
  Frac a <&&> Frac b = Frac (a + b)

------------------------------------------------------------

newtype Inv a = Inv {getInv :: a}
  deriving (Show,Eq,Ord,Num,Functor,Fractional)

instance Semiring a => Semiring (Inv a) where
  inv (Inv r) = Inv (inv r)
  Inv a <&&> Inv b = Inv (a <||> b)
  Inv a <||> Inv b = Inv (a <&&> b)

------------------------------------------------------------

data Range a = Short
             | Value a
             | Break
  deriving (Show,Eq,Functor)

instance Semiring s => Semiring (Range s) where
  Value r1 <&&> Value r2 = Value $ r1 <&&> r2
  Short <&&> r = r
  r <&&> Short = r
  _ <&&> Break = Break
  Break <&&> _ = Break

  inv Short = Break
  inv Break = Short
  inv (Value r) = Value (inv r)

boolToRange b = if b then Short else Break
rangeToBool = not . (== Break)
------------------------------------------------------------

data Element = R Double | C Double | L Double
             | Key Bool
             deriving Show

data Circuit a = Elem a
               | Par (Circuit a) (Circuit a)
               | Seq (Circuit a) (Circuit a) deriving Show

instance Semiring (Circuit a) where
  (<&&>) = Seq
  (<||>) = Par
  inv = id

foldCircuit f (Elem x) = f x
foldCircuit f (Par a b) = foldCircuit f a <||> foldCircuit f b
foldCircuit f (Seq a b) = foldCircuit f a <&&> foldCircuit f b

r = Elem . R
c = Elem . C
l = Elem . L
k = Elem . Key

s = (r 5 <&&> r 6) <||> r 10

resistance circ = getFrac <$> foldCircuit f circ
  where f (R x) = Value (Frac x)
        f (C _) = Break
        f (L _) = Short
        f (Key k) = boolToRange k

connected = rangeToBool . resistance

capacity circ = getFrac . getInv <$> foldCircuit f circ
  where f (C c) = Value $ Inv $ Frac c
        f (Key k) = boolToRange k
        f _ = Short

