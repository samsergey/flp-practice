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

data Circuit a = Zero
               | One
               | Elem a
               | Par (Circuit a) (Circuit a)
               | Seq (Circuit a) (Circuit a)
  deriving (Show, Eq, Functor)

instance DeMorgan (Circuit a) where
  zero = Zero
  one = One
  (<-->) = Seq
  (<||>) = Par
  inv = id

reduceDM c = case c of
  Zero -> zero
  One -> one
  Elem b -> b
  Par a b -> reduceDM a <||> reduceDM b
  Seq a b -> reduceDM a <--> reduceDM b

data Element = R Double
             | C Double
             | L Double
             deriving Show

resistanceAlg el =  case el of
  R r -> Value r
  C _ -> Break
  L _ -> Short

connectedAlg el = case el of
  R _ -> True
  C _ -> False
  L _ -> True
  
capacityAlg el = case el of
  R _ -> Short
  C c -> Value c
  L _ -> Short

impedanceAlg el = Value <$> case el of
  R r -> \w -> r :+ 0
  C c -> \w -> 0 :+ (-1)/(w*c)
  L l -> \w -> 0 :+ (w*l)  

reduceDMWith w f x = reduceDM $ (fmap w . f) <$> x

connected   = reduceDM . fmap connectedAlg
resistance  = fmap getFrac . reduceDMWith Frac resistanceAlg
impedance   = fmap (fmap getFrac) . reduceDMWith (fmap Frac) impedanceAlg
capacity    = fmap (getFrac . getDual) . reduceDMWith (Dual . Frac) capacityAlg

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
  where f (Insulation h l s) = l/(s*h)
        f (Surface a s) = 1/(s*a)

insulation c = fmap Elem . Insulation c
surface = fmap Elem . Surface

concrete = insulation 0.5
glass = insulation 1.05
air = insulation 0.024
pp = insulation 0.07

system = surface 1.5 3 <--> (wall 2 <||> window 1) <--> surface 6 3
  where
    wall = concrete 0.5 <--> pp 0.1
    window = glass 0.005 <--> air 0.01 <--> glass 0.005

flux s dT = dT / thermalResistance s

------------------------------------------------------------

distribute (Seq a (Par b c)) = distribute $ (a <--> b) <||> (a <--> c)
distribute (Seq (Par a b) c) = distribute $ (a <--> c) <||> (b <--> c)
distribute (Seq (Seq a b) c) = distribute $ a <--> (c <--> b)
distribute (Seq a b) = distribute a <--> distribute b
distribute (Par a b) = distribute a <||> distribute b
distribute x = x

edges :: Circuit a -> [(a,a)]
edges = foldMap links . reduceDM . fmap (\x -> [[x]]) . distribute
  where links lst = zip lst (tail lst)

a --> b = a <--> b
a <+> b = a <||> b
n = Elem

g :: Circuit Int
g = (n 1 --> (n 3 --> n 4)) <+> (n 2 --> (n 1 <+> n 3)) 

