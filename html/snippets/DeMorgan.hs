{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor,GeneralizedNewtypeDeriving #-}
import Data.Complex
import Data.List
import Data.Bool

class DeMorgan a where
  zero :: a
  one :: a
  inv :: a -> a
  (<->) :: a -> a -> a
  (<|>) :: a -> a -> a
  row :: Foldable t => t a -> a
  col :: Foldable t => t a -> a

  a <-> b = inv (inv a <|> inv b)
  a <|> b = inv (inv a <-> inv b)
  one = inv zero
  zero = inv one
  row = foldl1 (<->)
  col = foldl1 (<|>)
  {-# MINIMAL inv,((<->)|(<|>)),(zero|one) #-}

instance DeMorgan Bool where
  zero = False
  inv = not
  (<->) = (&&)

newtype Frac a = Frac {getFrac :: a}
  deriving (Show,Num,Fractional)

instance (Eq a, Fractional a) => DeMorgan (Frac a) where
  zero = 0
  inv = (1/)
  (<->) = (+)

newtype InvFrac a = InvFrac {getInvFrac :: a}
  deriving (Show,Num,Fractional)

instance (Eq a, Fractional a) => DeMorgan (InvFrac a) where
  zero = 0
  inv = (1/)
  (<|>) = (+)

instance DeMorgan [[a]] where
  zero = [[]]
  (<|>) = (++)
  inv = transpose

table f vals = (a <-> h) <|> (w <-> c)
  where
    a = [[" "]]
    h = [show <$> vals]
    w = inv h
    c = [ [show $ f x y | x <- vals ] | y <- vals ]

instance (DeMorgan b) => DeMorgan (a -> b) where
  inv f = inv <$> f
  f <|> g = (<|>) <$> f <*> g
  f <-> g = (<->) <$> f <*> g
  zero = pure zero
  one = pure one

------------------------------------------------------------

data Range a = Short
             | Value a
             | Break
  deriving (Show,Eq,Functor)

instance DeMorgan s => DeMorgan (Range s) where
  zero = Short

  Value r1 <-> Value r2 = Value $ r1 <-> r2
  Short <-> r = r
  r <-> Short = r
  _ <-> Break = Break
  Break <-> _ = Break

  inv Short = Break
  inv Break = Short
  inv (Value r) = Value (inv r)

------------------------------------------------------------

data Element = R Double
             | C Double
             | L Double
             deriving Show

data Semiring a = Zero | One | Elem a
  | Plus (Semiring a) (Semiring a)
  | Times (Semiring a) (Semiring a)
  | Inv (Semiring a)
  deriving (Show, Functor)

instance DeMorgan (Semiring a) where
  zero = Zero
  one = One
  (<->) = Times
  (<|>) = Plus
  inv = Inv

alg Zero = zero
alg One = one
alg (Elem b) = b
alg (Plus a b) = alg a <|> alg b
alg (Times a b) = alg a <-> alg b
alg (Inv a) = inv (alg a)

foldWith w f x = alg $ (fmap w . f) <$> x

r = Elem . R
c = Elem . C
l = Elem . L
k True = Zero
k False = One

resistance = fmap getFrac . foldWith Frac f
  where f (R r) = Value r
        f (C _) = Break
        f (L _) = Short

connected = alg . fmap f
  where f (R _) = True
        f (C _) = False
        f (L _) = True

capacity = fmap getInvFrac . foldWith InvFrac f
  where f (R _) = Short
        f (C c) = Value c
        f (L _) = Short

impedance s w = fmap getFrac . foldWith Frac f $ s
  where f (R r) = Value $ 0 :+ r
        f (C c) = Value $ 0 :+ (-1)/(w*c)
        f (L l) = Value $ 0 :+ (w*l)

