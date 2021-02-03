module Lab2 where

import Lab1 (mean)
import Data.List
import Data.Ord

data Circuit = R Double
               | Par Circuit Circuit
               | Seq Circuit Circuit deriving Show


resistance :: Circuit -> Resistance Double
resistance c = case c of
  R r -> Value r
  Seq c1 c2 -> resistance c1 <&&> resistance c2
  Par c1 c2 -> resistance c1 <||> resistance c2

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

Break <&&> _ = Break
Short <&&> r = r
Value a <&&> Value b = Value $ a + b
a <&&> b = b <&&> a

inv Short = Break
inv Break = Short
inv (Value x) = Value (1/x)

a <||> b = inv (inv a <&&> inv b)

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

data Worker = A | B | C | D deriving (Show, Eq)

worker w stage = case stage of
  1 -> case w of
         A -> R 3
         B -> R 2
         C -> R 4
         D -> R 3
  2 -> case w of
         A -> R 6
         B -> R 4
         C -> R 5
         D -> R 4
         
pairs = [ (w1, w2)
        | w1 <- [A, B, C, D]
        , w2 <- [A, B, C, D]
        , w1 /= w2]

opt = [ (p1, p2)
      | p1 <- pairs
      , p2 <- pairs ]

-- strategy s ((a,b),(c,d)) = case s of
--    1 -> (worker a 1 <||> worker b 1) <&&>
--         (worker c 2 <||> worker d 2)
--    2 -> (worker a 1 <&&> worker b 1) <||>
--         (worker c 2 <&&> worker d 2)
