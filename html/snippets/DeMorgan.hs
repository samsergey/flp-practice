{-# LANGUAGE
  FlexibleInstances,
  DeriveFunctor,
  GeneralizedNewtypeDeriving #-}
import Data.Complex
import Data.List
import Data.Bool
import Control.Applicative
import Control.Monad


class DeMorgan a where
  zero :: a
  one :: a
  inv :: a -> a
  (<-->) :: a -> a -> a
  (<||>) :: a -> a -> a

  a <--> b = inv (inv a <||> inv b)
  a <||> b = inv (inv a <--> inv b)
  one = inv zero
  zero = inv one

  row :: Foldable t => t a -> a
  col :: Foldable t => t a -> a
  row = foldl1 (<-->)
  col = foldl1 (<||>)
  {-# MINIMAL inv,((<-->)|(<||>)),(zero|one) #-}

instance DeMorgan Bool where
  zero = False
  inv = not
  (<-->) = (&&)

instance DeMorgan [[a]] where
  zero = [[]]
  (<||>) = (++)
  inv = transpose

table f vals = (a <--> h)
               <||>
               (inv h <--> c)
  where
    a = [[" "]]
    h = [show <$> vals]
    c = [show <$> [ f x y | x <- vals] | y <- vals]

showTable tbl = putStrLn . unlines $ unwords <$> tbl

newtype Frac a = Frac {getFrac :: a}
  deriving (Show, Num, Eq, Ord, Fractional, Functor)

instance Fractional a => DeMorgan (Frac a) where
  zero = 0
  inv = (1/)
  (<-->) = (+)

newtype Dual a = Dual {getDual :: a}
  deriving (Show, Functor,Num,Fractional,Eq,Ord)

instance DeMorgan a => DeMorgan (Dual a) where
  zero = Dual one
  one = Dual zero
  inv x = inv <$> x
  Dual a <||> Dual b = Dual $ a <--> b
  Dual a <--> Dual b = Dual $ a <||> b

------------------------------------------------------------

data Resistance a = Short
                  | Value a
                  | Break
  deriving (Show,Eq,Functor)

instance DeMorgan s => DeMorgan (Resistance s) where
  zero = Short

  Value r1 <--> Value r2 = Value $ r1 <--> r2
  Short <--> r = r
  r <--> Short = r
  _ <--> Break = Break
  Break <--> _ = Break

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
  deriving (Show, Eq, Functor)

instance Num a => Num (Semiring a) where
  fromInteger n = Elem (fromInteger n)
  (+) = (<||>)
  (*) = (<-->)
  signum = fmap signum
  abs = fmap abs
  negate = fmap negate

instance DeMorgan (Semiring a) where
  zero = Zero
  one = One
  (<-->) = Times
  (<||>) = Plus
  inv = id

reduceDM Zero = zero
reduceDM One = one
reduceDM (Elem b) = b
reduceDM (Plus a b) = reduceDM a <||> reduceDM b
reduceDM (Times a b) = reduceDM a <--> reduceDM b

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

capacity = fmap (getFrac . getDual) . reduceDMWith (Dual . Frac) f
  where f (R _) = Short
        f (C c) = Value c
        f (L _) = Short

impedance s w = fmap getFrac . reduceDMWith Frac f $ s
  where f (R r) = Value $ r :+ 0
        f (C c) = Value $ 0 :+ (-1)/(w*c)
        f (L l) = Value $ 0 :+ (w*l)

s = r 10 <--> ((r 2 <--> l 5e-13) <||> c 10e-9)

------------------------------------------------------------

insulation h s l = Dual . Frac $ l/(s*h)
surface a s = Dual . Frac $ 1/(s*a)

concrete = insulation 0.5
glass = insulation 1.05
air = insulation 0.024
pp = insulation 0.07
edge = surface 5

flux r dT = dT / getFrac (getDual r)

system = edge 1 <--> (wall <||> window) <--> edge 1
  where
    wall = concrete 2 0.5 <--> pp 2 0.1
    window = glass 1 0.05 <--> air 1 0.1 <--> glass 1 0.05

--heater = flux (surface 50 0.25) 60

------------------------------------------------------------

distribute (Times a (Plus b c)) = distribute $ (a <--> b) <||> (a <--> c)
distribute (Times (Plus a b) c) = distribute $ (a <--> c) <||> (b <--> c)
distribute (Times (Times a b) c) = distribute $ a <--> (c <--> b)
distribute (Times a b) = distribute a <--> distribute b
distribute (Plus a b) = distribute a <||> distribute b
distribute x = x

edges :: Semiring a -> [(a,a)]
edges = foldMap links . reduceDM . fmap (\x -> [[x]]) . distribute
  where links lst = zip lst (tail lst)

a --> b = a <--> b
a <+> b = a <||> b

g :: Semiring Int
g = (1 --> (3 --> 4)) <+> (2 --> (1 <+> 3)) 
