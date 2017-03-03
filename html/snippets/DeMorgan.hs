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


data Semiring a = Zero
                | One
                | Elem a
                | Plus (Semiring a) (Semiring a)
                | Times (Semiring a) (Semiring a)
                | Inv (Semiring a)
  deriving (Show, Functor)

instance Num a => Num (Semiring a) where
  fromInteger n = Elem (fromInteger n)
  (+) = (<|>)
  (*) = (<->)
  signum = fmap signum
  abs = fmap abs
  negate = fmap negate

instance DeMorgan (Semiring a) where
  zero = Zero
  one = One
  (<->) = Times
  (<|>) = Plus
  inv = Inv

reduceDM Zero = zero
reduceDM One = one
reduceDM (Elem b) = b
reduceDM (Plus a b) = reduceDM a <|> reduceDM b
reduceDM (Times a b) = reduceDM a <-> reduceDM b
reduceDM (Inv a) = inv (reduceDM a)

reduceDMWith w f x = reduceDM $ (fmap w . f) <$> x

r = Elem . R
c = Elem . C
l = Elem . L
k True = Zero
k False = One

resistance = fmap getFrac . reduceDMWith Frac f
  where f (R r) = Value r
        f (C _) = Break
        f (L _) = Short

connected = reduceDM . fmap f
  where f (R _) = True
        f (C _) = False
        f (L _) = True

capacity = fmap getInvFrac . reduceDMWith InvFrac f
  where f (R _) = Short
        f (C c) = Value c
        f (L _) = Short

impedance s w = fmap getFrac . reduceDMWith Frac f $ s
  where f (R r) = Value $ 0 :+ r
        f (C c) = Value $ 0 :+ (-1)/(w*c)
        f (L l) = Value $ 0 :+ (w*l)

------------------------------------------------------------

insl h s l = Elem (l/(s*h))
surf a s = Elem (1/(s*a))
concrete = insl 0.5
glass = insl 1.05
air = insl 0.024
pp = insl 0.07
edge = surf 5

flux s dT = 40 / (getInvFrac . reduceDM $ InvFrac <$> s)

system = edge 1 <-> (wall <|> window) <-> edge 1
  where
    wall = concrete 2 0.5 <-> pp 2 0.1
    window = glass 1 0.05 <-> air 1 0.1 <-> glass 1 0.05

heater = flux (surf 50 0.25) 60

------------------------------------------------------------

distribute (Times a (Plus b c)) = distribute $ (a <-> b) <|> (a <-> c)
distribute (Times (Plus a b) c) = distribute $ (a <-> c) <|> (b <-> c)
distribute (Times a b) = distribute a <-> distribute b
distribute (Plus a b) = distribute a <|> distribute b
distribute x = x


collect :: Semiring a -> [[a]]
collect = reduceDM . fmap (\x -> [[x]]) . distribute

links i = nub . concat . filter (i `elem`) . collect

a --> b = a <-> b
a <+> b = a <|> b

