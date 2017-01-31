import Data.Complex

repeat' n k = const k <$> [1..n]

sumsqr a b = sum $ (^2) <$> [a..b]

series n = (\i -> (1+i/10)*((-1)**i)) <$> [0..n-1]

e = takeWhile (> 1e-10) $ (\i -> 1/fact i) <$> [0..] 

fact i = product [1..i]

sin' x = takeWhile ((> 1e-10).abs) $ (\i -> (-1**((i-1) / 2))*x**i/fact i) <$> [1,3..]

area r = length $ filter (< r^2) $ concat $ (\x -> (\y -> x^2 + y^2) <$> [1..r]) <$> [1..r]

toBase d = map (`mod` d) . takeWhile (> 0) . iterate (`div` d)
fromBase d = foldr (\x res -> d*res + x) 0

secant f x1 x2 = converge 1e-11 $ take 50 $ roots
  where g x1 x2 = (f x2 * x1 - f x1 * x2)/(f x2 - f x1)
        roots = x1 : x2 : zipWith g roots (tail roots)

converge eps [] = Nothing
converge eps [x] = Nothing
converge eps (x:y:xs)
  | abs (x-y)<eps = Just y
  | otherwise = converge eps (y:xs)

join sep str = foldr (\el res -> sep ++ el ++ res) "" str

pascal = iterate (\x -> zipWith (+) (0:x) (tail $ cycle (0:x))) [1]

data Circuit a = R a | C a | L a
               | Key Bool
               | Par (Circuit a) (Circuit a)
               | Seq (Circuit a) (Circuit a) deriving Show

data Resistance a = Short | Break | Value a deriving Show

Value r1 <&&> Value r2 = Value $ r1 + r2
Short <&&> r = r
r <&&> Short = r
_ <&&> Break = Break
Break <&&> _ = Break

inv Short = Break
inv Break = Short
inv (Value r) = Value (1/r)

a <||> b = inv ((inv a) <&&> (inv b))

resistance (R r) = Value r
resistance (C c) = Break
resistance (L l) = Short
resistance (Key True) = Short
resistance (Key False) = Break
resistance (Par c1 c2) = resistance c1 <||> resistance c2
resistance (Seq c1 c2) = resistance c1 <&&> resistance c2

impedance (R r) _ = Value $ r :+ 0
impedance (C c) w = Value $ 1/(0 :+ w*c)
impedance (L l) w = Value $ (0 :+ w*l)
impedance (Key True) w = Short
impedance (Key False) w = Break
impedance (Par c1 c2) w = impedance c1 w <||> impedance c2 w
impedance (Seq c1 c2) w = impedance c1 w <&&> impedance c2 w

capacity c = case c of
  C c -> Value c
  Key False -> Break
  Par c1 c2 -> capacity c1 <&&> capacity c2
  Seq c1 c2 -> capacity c1 <||> capacity c2
  _ -> Short

connected x = case resistance x of
  Break -> False
  _ -> True

c k1 k2 = (Key k1 `Seq` R 7) `Par` (Key k2 `Seq` (R 12 `Par` (R 1 `Seq` R 5)))

