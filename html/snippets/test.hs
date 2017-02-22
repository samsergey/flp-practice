{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
import Data.Complex
import Control.Applicative
import Data.List.Split
import Data.List
import Data.Monoid
import Control.Monad
import Data.Ratio

repeat' :: (Num b1, Enum b1) => b1 -> b -> [b]
repeat' n k = const k <$> [1..n]

sumsqr :: (Num a, Enum a) => a -> a -> a
sumsqr a b = sum $ (^2) <$> [a..b]

series :: (Floating b, Enum b) => b -> [b]
series n = (\i -> (1+i/10)*((-1)**i)) <$> [0..n-1]

e :: [Double]
e = takeWhile (> 1e-10) $ (\i -> 1/fact i) <$> [0..] 

fact :: (Num a, Enum a) => a -> a
fact i = product [1..i]

sin' :: (Ord a, Floating a, Enum a) => a -> [a]
sin' x = takeWhile ((> 1e-10).abs) $ (\i -> (-1**((i-1) / 2))*x**i/fact i) <$> [1,3..]

area :: (Ord a, Num a, Enum a) => a -> Int
area r = length $ filter (< r^2) $ concat $ (\x -> (\y -> x^2 + y^2) <$> [1..r]) <$> [1..r]

--toBase :: Integral a => a -> a -> [a]
--toBase d = reverse . map (`mod` d) . takeWhile (> 0) . iterate (`div` d)

--toBase d = map (`mod` d) . takeWhile (> 0) . iterate (`div` d)

toBase base n
  | n < base = [n]
  | otherwise = r : toBase base q
  where
    (q, r) = n `divMod` base

-- toBase base = unfoldr modDiv
--   where modDiv 0 = Nothing
--         modDiv n = let (q, r) = (n `divMod` base)
--                    in Just (r, q) 


fromBase :: (Foldable t, Num a) => a -> t a -> a
fromBase d = foldl (\res x -> d*res + x) 0

secant :: (Ord a, Fractional a) => (a -> a) -> a -> a -> Maybe a
secant f x1 x2 = converge 1e-11 $ take 50 $ roots
  where g x1 x2 = (f x2 * x1 - f x1 * x2)/(f x2 - f x1)
        roots = x1 : x2 : zipWith g roots (tail roots)

converge :: (Ord a, Num a) => a -> [a] -> Maybe a
converge eps [] = Nothing
converge eps [x] = Nothing
converge eps (x:y:xs)
  | abs (x-y)<eps = Just y
  | otherwise = converge eps (y:xs)

join' :: Foldable t => String -> t String -> String
join' sep str = foldr (\el res -> sep ++ el ++ res) "" str

pascal :: [[Integer]]
pascal = iterate (\x -> zipWith (+) (0:x) (tail $ cycle (0:x))) [1]

data Circuit = R Double | C Double | L Double
               | Key Bool
               | Par Circuit Circuit
               | Seq Circuit Circuit deriving Show

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

resistance :: Circuit -> Resistance Double
resistance (R r) = Value r
resistance (C c) = Break
resistance (L l) = Short
resistance (Key True) = Short
resistance (Key False) = Break
resistance (Par c1 c2) = resistance c1 <||> resistance c2
resistance (Seq c1 c2) = resistance c1 <&&> resistance c2

impedance :: Circuit -> Double -> Resistance (Complex Double)
impedance (R r) _ = Value $ r :+ 0
impedance (C c) w = Value $ 1/(0 :+ w*c)
impedance (L l) w = Value $ (0 :+ w*l)
impedance (Key True) w = Short
impedance (Key False) w = Break
impedance (Par c1 c2) w = impedance c1 w <||> impedance c2 w
impedance (Seq c1 c2) w = impedance c1 w <&&> impedance c2 w

connected :: Circuit -> Bool
connected x = case resistance x of
  Break -> False
  _ -> True

c :: Bool -> Bool -> Circuit
c k1 k2 = (Key k1 `Seq` R 7) `Par` (Key k2 `Seq` (R 12 `Par` (R 1 `Seq` R 5)))

readSay :: [Int] -> [Int]
readSay [] = []
readSay [x] = [1,x]
readSay (x:xs) = let (a,b) = break (/=x) (x:xs) in [length a,x]++readSay b

minCount :: (Ord t, Num t1) => [t] -> (t, t1)
minCount [] = error "No elements"
minCount (x:xs) = foldl count (x,1) xs
  where
    count (r, c) x  = case compare x r of
      LT -> (x,1)
      EQ -> (r,c+1)
      GT -> (r,c)

solveVolvo :: [String]
solveVolvo = [ mconcat[show volvo, "+", show fiat, "=", show motor]
        | v <- [1..9]
        , o <- [0..9], o /= v
        , l <- [0..9], not $ l `elem` [v,o]
        , f <- [1..9], not $ f `elem` [v,o,l]
        , i <- [0..9], not $ i `elem` [v,o,l,f]
        , a <- [0..9], not $ a `elem` [v,o,l,f,i]
        , t <- [0..9], not $ t `elem` [v,o,l,f,i,a]
        , m <- [1..9], not $ m `elem` [v,o,l,f,i,a,t]
        , r <- [0..9], not $ r `elem` [v,o,l,f,i,a,t,m]
        , let volvo = fromBase 10 [v,o,l,v,o]
              fiat = fromBase 10 [f,i,a,t]
              motor = fromBase 10 [m,o,t,o,r]
        , volvo + fiat == motor]

main :: IO ()
main = mapM_ putStrLn solveVolvo



multLong :: Int -> [Int] -> [Int]
multLong x lst = dropWhile (==0) $ init $ snd <$> scanr addShift (0,0) (0:lst)
  where
    addShift el (q,_) = (x*el+q) `divMod` (10^16)

factLong :: Int -> [Int]
factLong n = foldr multLong [1] [1..n]

data Client = Client { name :: String
                     , age :: Int
                     , address :: Maybe Address } deriving Show
data Address = Address {street :: String, house :: Int} deriving Show

table = concat [ "Johny Mitchell, 35, Tony lane, 6\n"
               , "Jim Morisson, 27, Ivy ave, 32\n"
               , "Marty Mc'Fly, 18, Lion est., 16\n"
               , "Woody Woodepecker, 63, -,-"]

readCSV = fmap (splitOn ",") . splitOn "\n"


class Alternative f => Failable f where
  message :: String -> f a
  message _ = empty   -- определение, используемое по умолчанию

instance Failable Maybe
instance Failable []
instance Failable (Either String) where message = Left
instance Failable IO where message = error

instance Monoid m => Alternative (Either m) where
  empty = Left mempty
  Left x  <|> Left y = Left (x <> y)
  Left _  <|> x      = x 
  Right x <|> _      = Right x 

sqrtA 0 = pure 0
sqrtA x | x < 0 = message "Negative argument!"
        | x > 0 = pure r <|> pure (-r) where r = sqrt x 

bisectionA :: (Alternative f, Eq a)
           => (Double -> a) -> Double -> Double -> f Double
bisectionA f a b | f a == f b = empty
                 | abs (a - b) < 1e-14 * abs c = pure c
                 | otherwise = bisectionA f a c <|> bisectionA f c b
  where c = (a + b) / 2

bisection
  :: (Failable f, Eq a) =>
     (Double -> a) -> Double -> Double -> f Double
bisection f a b = bisectionA f a b <|> message "No root at given interval."

findRoot :: (Failable f, Eq a)
         => (Double -> a) -> [Double] -> f Double
findRoot f [] = message "The mesh is empty"
findRoot f [x] = message "The mesh has only one point"
findRoot f  mesh = go mesh <|> message "There was no roots found"
  where go [] = empty
        go [_] = empty
        go (x:y:xs) = bisectionA f x y <|> go (y:xs)

        
