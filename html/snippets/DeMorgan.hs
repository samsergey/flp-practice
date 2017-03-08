{-# LANGUAGE
  FlexibleInstances,
  FlexibleContexts,
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
  deriving (Show, Functor,Eq,Ord)

instance DeMorgan a => DeMorgan (Dual a) where
  zero = Dual one
  one = Dual zero
  inv x = inv <$> x
  Dual a <||> Dual b = Dual $ a <--> b
  Dual a <--> Dual b = Dual $ a <||> b

instance DeMorgan b => DeMorgan (a -> b) where
  zero = pure zero
  one = pure one
  inv = fmap inv
  (<||>) = liftA2 (<||>)
  (<-->) = liftA2 (<-->)


------------------------------------------------------------

data Lumped a = Short
              | Value a
              | Break
  deriving (Show,Eq,Functor)

instance DeMorgan s => DeMorgan (Lumped s) where
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

data Circuit a = Zero
               | One
               | Elem a
               | Plus (Circuit a) (Circuit a)
               | Times (Circuit a) (Circuit a)
  deriving (Show, Eq, Functor)

instance DeMorgan (Circuit a) where
  zero = Zero
  one = One
  (<-->) = Times
  (<||>) = Plus
  inv = id

reduceDM c = case c of
  Zero -> zero
  One -> one
  Elem b -> b
  Plus a b -> reduceDM a <||> reduceDM b
  Times a b -> reduceDM a <--> reduceDM b

reduceDMWith w f x = reduceDM $ (fmap w . f) <$> x

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

impedance w = fmap getFrac . reduceDMWith Frac (Value . f)
  where f (R r) = r :+ 0
        f (C c) = 0 :+ (-1)/(w*c)
        f (L l) = 0 :+ (w*l)

r = Elem . R
c = Elem . C
l = Elem . L
k True = Zero
k False = One

s k1 = r 10 <--> ((r 2 <--> l 5e-3 <--> k k1) <||> c 10e-9)

------------------------------------------------------------

data Layer = Insulation { coefficient :: Double
                        , thickness :: Double
                        , area :: Double }
           | Surface { coefficient :: Double
                     , area :: Double } deriving Show

thermalResistance :: Circuit Layer -> Double
thermalResistance = getFrac . reduceDM . fmap (Frac . f)
  where f (Insulation h s l) = l/(s*h)
        f (Surface a s) = 1/(s*a)

insulation c = fmap Elem . Insulation c
surface h = Elem . Surface h

concrete = insulation 0.5
glass = insulation 1.05
air = insulation 0.024
pp = insulation 0.07
edge = surface 5

system = edge 3 <--> (wall 2 <||> window 1) <--> edge 3
  where
    wall = concrete 0.5 <--> pp 0.1
    window = glass 0.05 <--> air 0.1 <--> glass 0.05

flux s dT = dT / thermalResistance s

------------------------------------------------------------

distribute (Times a (Plus b c)) = distribute $ (a <--> b) <||> (a <--> c)
distribute (Times (Plus a b) c) = distribute $ (a <--> c) <||> (b <--> c)
distribute (Times (Times a b) c) = distribute $ a <--> (c <--> b)
distribute (Times a b) = distribute a <--> distribute b
distribute (Plus a b) = distribute a <||> distribute b
distribute x = x

edges :: Circuit a -> [(a,a)]
edges = foldMap links . reduceDM . fmap (\x -> [[x]]) . distribute
  where links lst = zip lst (tail lst)

a --> b = a <--> b
a <+> b = a <||> b
n = Elem

g :: Circuit Int
g = (n 1 --> (n 3 --> n 4)) <+> (n 2 --> (n 1 <+> n 3)) 
