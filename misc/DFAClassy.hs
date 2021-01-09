{-#language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
import Data.List (transpose, unfoldr, tails, inits)

------------------------------------------------------------

class Machine m s i | m -> i, m -> s where
  alphabet :: m -> i -> Bool
  delta :: m -> s -> i -> s
  start :: m -> s
  stop :: m -> s -> Bool
  final :: m -> s -> Bool
  
scan :: Machine m s i => m -> [i] -> [(s, i)]
scan m xs = takeWhile (not . stop m . fst)
            $ zip states inputs
  where
    inputs = takeWhile (alphabet m) xs
    states = tail $ scanl (delta m) (start m) inputs
      
run :: Machine m s i => m -> [i] -> s
run m xs = case scan m xs of
               [] -> start m
               res -> fst (last res)

test :: Machine m s i => m -> [i] -> Bool
test m = any (\(s,_) -> final m s) . scan m

prefix m xs = case scan m xs of
  [] -> Nothing
  res -> Just ((fst (last res), snd <$> res), drop (length res) xs)

------------------------------------------------------------

data FSMachine s i = FSMachine [i] (s -> i -> s) s [s] [s]

instance (Eq s, Eq i) => Machine (FSMachine s i) s i where
  alphabet (FSMachine i _ _ _ _) = (`elem` i)
  delta (FSMachine _ d _ _ _) = d
  start (FSMachine _ _ s0 _ _) = s0
  stop (FSMachine _ _ _ s _) = (`elem` s)
  final (FSMachine _ _ _ _ f) = (`elem` f)


mod3 :: FSMachine Int Int 
mod3 = FSMachine [0,1] f 0 [] [0,1,2]
  where f 0 0 = 0
        f 0 1 = 1
        f 1 0 = 2
        f 1 1 = 0
        f 2 0 = 1
        f 2 1 = 2

toBase b = reverse
         . map (`mod` b)
         . takeWhile (> 0)
         . iterate (`div` b)

abba = FSMachine "ab" f 0 [-1] [4]
  where f s x = case (s, x) of
          (0, 'a') -> 1
          (0, 'b') -> -1
          (1, 'a') -> -1
          (1, 'b') -> 2
          (2, 'a') -> 4
          (2, 'b') -> 2
          (4, _) -> 4

aaa = FSMachine "ab" f 0 [-1] [4]
  where f s x = case (s, x) of
          (0, 'a') -> 1
          (0, 'b') -> -1
          (1, 'a') -> 2
          (1, 'b') -> -1
          (2, 'a') -> 3
          (2, 'b') -> -1
          (3, _) -> 4
          (s,_) -> s

bbb =  FSMachine "ab" f 0 [-1] [3]
  where f s x = case (s, x) of
          (0, 'a') -> 0
          (0, 'b') -> 1
          (1, 'a') -> 0
          (1, 'b') -> 2
          (2, 'a') -> 0
          (2, 'b') -> 3
          (3, 'a') -> 0
          (3, 'b') -> 3

same = FSMachine "ab" f "start" [] ["a2","b2"]
  where f s x = case (s, x) of
          ("start", 'a') -> "a1"
          ("start", 'b') -> "b1"
          ("a1", 'a') -> "a2"
          ("a1", 'b') -> "a1"
          ("a2", 'a') -> "a2"
          ("a2", 'b') -> "a1"
          ("b1", 'a') -> "b1"
          ("b1", 'b') -> "b2"
          ("b2", 'a') -> "b1"
          ("b2", 'b') -> "b2"

evena = FSMachine "ab" f 0 [] [2]
  where f s x = case (s, x) of
          (0, 'a') -> 1
          (1, 'a') -> 2
          (2, 'a') -> 1
          (s, 'b') -> s

------------------------------------------------------------

data PMachine s i = PMachine ([s] -> i -> [s]) ([s] -> Bool)

instance (Eq s, Eq i) => Machine (PMachine s i) [s] i where
  alphabet _ = const True
  delta (PMachine d _) = d
  start _ = []
  stop _ = const False
  final (PMachine _ f) = f

brackets = PMachine f null
  where f ('(':s) ')' = s
        f ('[':s) ']' = s
        f ('{':s) ']' = s
        f s x | x `elem` "[]{}()" = x:s
              | otherwise = s

calcRPN = PMachine f (const True)
  where f (x:y:s) "+" = (x+y):s
        f (x:y:s) "-" = (y-x):s
        f (x:y:s) "*" = (x*y):s
        f (x:y:s) "/" = (y `div` x):s
        f s n = read n : s

-- ------------------------------------------------------------

-- runPT :: ([s] -> i -> ([s], [o]))
--       -> ([s] -> [o])
--       -> [i] -> [o]
-- runPT step finish = go []
--   where go s [] = finish s
--         go s (h:t) = let (s', res) = step s h
--                      in res ++ go s' t

-- ------------------------------------------------------------

data Token = S | N | O | P | E deriving (Show, Eq)
  
lexer = FSMachine "0123456789+-*/()" f S [E] [O,P]
  where f S x | x `elem` digits = N
              | x `elem` "+-*/" = O
              | x `elem` "()"   = P
              | otherwise       = E
        f N x | x `elem` digits = N
              | otherwise       = E
        f s _                   = E
        
        digits = "0123456789"

-- ------------------------------------------------------------
-- dijkstra :: String -> [String]
-- dijkstra = runPT step id . tokenize
--   where
--     tokenize = unfoldr (prefixFSM lexer)
    
--     step s x = case x of
--       (N, n)  -> (s, [n])
--       (O, op)  -> pushOperator s op
--       (P, "(") -> ("(":s, [])
--       (P, ")") -> closePar s

--     pushOperator s x = (x:s', o)
--       where (o, s') = span (\y -> prec x < prec y) s

--     closePar s = case span (/= "(") s of
--       (_ ,[]) -> error "Unmatched parenthesis"
--       (o, s') -> (tail s', o)

--     prec x = case x of
--       "*" -> 2
--       "/" -> 2
--       "+" -> 1
--       "-" -> 1
--       "(" -> 0

-- -----------------------------------------------------------                                       
-- data M = M [[Double]] | I deriving Show

-- outer f l1 l2 = [[f x y | x <- l2 ] | y <- l1]
-- dot v1 v2 = sum $ zipWith (*) v1 v2

-- instance Semigroup M where
--   m <> I = m
--   I <> m = m
--   M m1 <> M m2 = M $ outer dot m1 (transpose m2)

-- instance Monoid M where
--   mempty = I

-- toBase2  = unfoldr f
--   where f 0 = Nothing
--         f n = let (n', r) = divMod n 2 in Just (r, n')

-- iterate1 f x = x : iterate1 f (f x)

-- iterate2 f = unfoldr (\x -> Just (x, f x))

-- replicate1 n x = unfoldr (\n -> if n > 0 then Just (x, n-1) else Nothing) n

-- cycle1 c = c ++ cycle1 c

-- cycle2 c = unfoldr g c
--   where g [x] = Just (x, c)
--         g (h:t) = Just (h, t)
