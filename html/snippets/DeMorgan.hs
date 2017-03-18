{-# LANGUAGE
  DeriveFunctor,
  FlexibleInstances,
  TypeApplications,
  GeneralizedNewtypeDeriving #-}
import Data.Complex
import Data.List (transpose)
import Control.Applicative hiding ((<|>))
-- import Data.Foldable
-- import Data.Monoid hiding (Dual(..))
import Test.QuickCheck
-- import Control.Arrow

class DeMorgan a where
  {-# MINIMAL inv,((<->)|(<|>)),(zero|one) #-}
  inv :: a -> a

  zero :: a
  zero = inv one

  one :: a
  one = inv zero

  (<->) :: a -> a -> a
  a <-> b = inv (inv a <|> inv b)

  (<|>) :: a -> a -> a
  a <|> b = inv (inv a <-> inv b)

instance DeMorgan Bool where
  zero = False
  inv = not
  (<->) = (&&)

newtype Frac a = Frac { getFrac :: a}
  deriving (Show, Num, Eq, Ord, Fractional, Functor, Arbitrary)

instance Fractional a => DeMorgan (Frac a) where
  zero = 0
  inv = (1/)
  (<->) = (+)

deMorganLaw1 eq a b = inv (a <|> b) `eq` (inv a <-> inv b)
deMorganLaw2 eq a b = inv (a <-> b) `eq` (inv a <|> inv b)
deMorganLaw3 eq a = one <-> a `eq` a && a <-> one `eq` a
deMorganLaw4 eq a = (zero <|> a) `eq` a && (a <|> zero) `eq` a

a ~~ b = a == b || abs (a-b) < 1e-14 || abs (a - b) <= abs (a + b) * 1e-14 

newtype Dual a = Dual { getDual :: a}
  deriving (Show, Functor, Eq, Ord, Num, Fractional)

instance DeMorgan a => DeMorgan (Dual a) where
  zero = Dual one
  one = Dual zero
  inv x = inv <$> x
  Dual a <|> Dual b = Dual $ a <-> b
  Dual a <-> Dual b = Dual $ a <|> b

instance DeMorgan b => DeMorgan (a -> b) where
  zero = pure zero
  one = pure one
  inv = fmap inv
  (<|>) = liftA2 (<|>)
  (<->) = liftA2 (<->)

instance (DeMorgan a,DeMorgan b) => DeMorgan (a, b) where
  zero = (zero,zero)
  one = (one,one)
  inv (a,b) = (inv a, inv b)
  (a,a') <|> (b,b') = (a <|> b, a' <|> b')
  (a,a') <-> (b,b') = (a <-> b, a' <-> b')


newtype Monoidal a = Monoidal { getMonoidal :: a }
  deriving (Show, Monoid, Num, Fractional)

instance Monoid b => DeMorgan (Monoidal b) where
  zero = mempty
  one = mempty
  inv = id
  (<|>) = mappend
  (<->) = mappend

------------------------------------------------------------

data Lumped a = Short
              | Value a
              | Break
  deriving (Show, Functor)


instance DeMorgan s => DeMorgan (Lumped s) where
  zero = Break

  Value x1 <-> Value x2 = Value $ x1 <-> x2
  Short <-> x = x
  x <-> Short = x
  _ <-> Break = Break
  Break <-> _ = Break

  inv Short = Break
  inv Break = Short
  inv (Value x) = Value (inv x)

------------------------------------------------------------

data Circuit a = Zero
               | One
               | Elem a
               | Inv (Circuit a)
               | Par (Circuit a) (Circuit a)
               | Seq (Circuit a) (Circuit a)
  deriving (Show, Functor)

instance DeMorgan (Circuit a) where
  zero = Zero
  one = One
  (<->) = Seq
  (<|>) = Par
  inv = Inv

reduce :: DeMorgan a => Circuit a -> a
reduce circ = case circ of
  Zero -> zero
  One -> one
  Elem b -> b
  Inv a -> inv (reduce a)
  Par a b -> reduce a <|> reduce b
  Seq a b -> reduce a <-> reduce b

data Element = R Double
             | C Double
             | L Double
             deriving Show

reduceMap :: DeMorgan c => (a -> c) -> Circuit a -> c
reduceMap f = reduce . fmap f

connected :: Circuit Element -> Bool
connected = reduceMap f
  where
    f el = case el of
      R _ -> True
      C _ -> False
      L _ -> True

resistance :: Circuit Element -> Lumped Double
resistance = fmap getFrac . reduceMap (fmap Frac . f)
  where
    f el = case el of
      R r -> Value r
      C _ -> Break
      L _ -> Short

impedance :: Double -> Circuit Element -> Lumped (Complex Double)
impedance w = fmap getFrac . reduceMap (fmap Frac . f)
  where
    f el = Value $ case el of
      R r -> r :+ 0
      C c -> 1 / (0 :+ w*c)
      L l -> 0 :+ w*l

capacity :: Circuit Element -> Lumped Double
capacity = fmap (getFrac . getDual) . reduceMap (fmap (Dual . Frac) . f)
  where
    f el = case el of
      R _ -> Short
      C c -> Value c
      L _ -> Short

res = Elem . R
cap = Elem . C
coil = Elem . L
key True = One
key False = Zero

s k = res 10 <-> ((res 2 <-> coil 5e-3 <-> key k) <|> cap 10e-9)

------------------------------------------------------------

data Layer = Insulation { coefficient :: Double
                        , thickness :: Double
                        , area :: Double }
           | Surface    { coefficient :: Double
                        , area :: Double } deriving Show

thermalResistance :: Circuit Layer -> Double
thermalResistance = getFrac . reduceMap (Frac . f)
  where
    f el = case el of
      Insulation h l s -> l/(s*h)
      Surface a s -> 1/(s*a)

insulation :: Double -> Double -> Double -> Circuit Layer
insulation h = fmap Elem . Insulation h

surface :: Double -> Double -> Circuit Layer
surface = fmap Elem . Surface

concrete, glass, air, pp :: Double -> Double -> Circuit Layer
concrete = insulation 0.5
glass = insulation 1.05
air = insulation 0.024
pp = insulation 0.07

system :: Circuit Layer
system = surface 1.5 3 <-> (wall 2.5 <|> window 0.5) <-> surface 6 3
  where
    wall = concrete 0.5 <-> pp 0.2
    window = glass 0.005 <-> air 0.01 <-> glass 0.005 <-> air 0.01 <-> glass 0.005

instance DeMorgan [[a]] where
  zero = [[]]
  (<|>) = (++)
  inv = transpose


------------------------------------------------------------

-- distribute :: Circuit t -> Circuit t
-- distribute (Seq a (Par b c)) = distribute $ (a <-> b) <|> (a <-> c)
-- distribute (Seq (Par a b) c) = distribute $ (a <-> c) <|> (b <-> c)
-- distribute (Seq (Seq a b) c) = distribute $ a <-> (c <-> b)
-- distribute (Seq a b) = distribute a <-> distribute b
-- distribute (Par a b) = distribute a <|> distribute b
-- distribute x = x

-- --edges :: Circuit t -> [[t]]
-- edges = foldMap links . reduceMap (\x -> [[x]]) . distribute
--   where links lst = zip lst (tail lst)

-- (-->), (<+>) :: DeMorgan a => a -> a -> a
-- a --> b = a <-> b
-- a <+> b = a <|> b
-- n = Elem


-- g = (n 1 --> n 4) <+> (n 1 <+> (n 3 --> n 4)) <+> (n 2 --> (n 1 <+> n 3)) 

-- -- -- main = pure ()

-- layerRes el = case el of
--   Insulation h l s -> l/(s*h)
--   Surface a s -> 1/(s*a)
------------------------------------------------------------

-- data Table a = Table (NE.NonEmpty (NE.NonEmpty a)) | Empty deriving (Eq,Show)

-- toListT Empty = []
-- toListT (Table tbl) = NE.toList <$> NE.toList tbl

-- instance DeMorgan (Table a) where
--   zero = Empty
--   Empty <|> a  = a
--   a  <|> Empty = a
--   a <|> b  = table $ toListT a ++ toListT b
-- --  (<->) = zipWith (++)
--   inv Empty = Empty
--   inv (Table a) = Table (NE.transpose a)


-- table :: [[a]] -> Table a
-- table [] = Empty
-- table ([]:_) = Empty
-- table lst = Table $ NE.fromList $ NE.fromList <$> lst

-- --tabulate :: (Show a1, Show a) => (a1 -> a1 -> a) -> [a1] -> Table String
-- tabulate f vals = (a <-> h)
--                <|>
--                (inv h <-> c)
--   where
--     a = table [[" "]]
--     h = table [show <$> vals]
--     c = table [show <$> [ f x y | x <- vals] | y <- vals]

-- showTable :: [[String]] -> IO ()
-- showTable tbl = putStrLn . unlines $ unwords <$> tbl

-- -- dm1, dm2 :: Table Int -> Table Int -> Bool
-- -- dm1 a b = (a <-> b) == inv (inv a <|> inv b)
-- -- dm2 a b = (a <|> b) == inv (inv a <-> inv b)

-- instance Arbitrary (Table Int) where
--   arbitrary = table <$> (arbitrary :: Gen [[Int]]) 

newtype Fuzzy = Fuzzy { getFuzzy :: Double }
  deriving (Show, Num, Fractional, Ord, Eq)

instance DeMorgan Fuzzy where
  zero = 0
  inv x = fuzzy $ 1 - x
  a <|> b = fuzzy $ max a b

fuzzy x = 0 `max` x `min` 1

instance Arbitrary Fuzzy where
  arbitrary = Fuzzy <$> (arbitrary :: Gen Double) 
