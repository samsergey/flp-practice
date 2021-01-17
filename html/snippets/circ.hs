{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
import Data.Foldable

data Element = R Double
             | C Double
             | L Double
             | Key Bool
  deriving Show

data P a = P [S a] | Pv a deriving Show
data S a = S [P a] | Sv a deriving Show



data Par a = Par {getPar :: a} deriving Show
instance (Fractional a,Num a) => Monoid (Par (Resistance a)) where
  mempty = Par Break
  Par a `mappend` Par b = Par $ a <||> b

data Seq a = Seq {getSeq :: a} deriving Show
instance (Fractional a,Num a) => Monoid (Seq (Resistance a)) where
  mempty = Seq Break
  Seq a `mappend` Seq b = Seq $ a <||> b



data Resistance a = Short | Break | Value a deriving Show

(<&&>) :: Num a => Resistance a -> Resistance a -> Resistance a
Value r1 <&&> Value r2 = Value $ r1 + r2
Short <&&> r = r
r <&&> Short = r
_ <&&> Break = Break
Break <&&> _ = Break

inv :: Fractional a => Resistance a -> Resistance a
inv Short = Break
inv Break = Short
inv (Value r) = Value (1/r)

(<||>) :: Fractional a => Resistance a -> Resistance a -> Resistance a
a <||> b = inv ((inv a) <&&> (inv b))
