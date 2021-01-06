import Data.List (transpose)
import Data.Semigroup
import Data.Monoid
import Text.Printf

  
data DFA s i o = DFA
  { symbols :: [i]
  , func :: s -> i -> s
  , start :: s
  , result :: s -> o }

runDFA :: Eq i => DFA s i o -> [i] -> o
runDFA (DFA is f s0 post) = post . foldl f s0 . filter (`elem` is)

mod3 = DFA [0,1] f 0 id
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

abba = DFA "ab" f 0 (== 4)
  where f s x = case (s, x) of
          (0, 'a') -> 1
          (0, 'b') -> 0
          (1, 'a') -> 0
          (1, 'b') -> 2
          (2, 'a') -> 0
          (2, 'b') -> 3
          (3, 'a') -> 4
          (3, 'b') -> 0
          (4, _) -> 4

aaa = DFA "ab" f 0 (== 3)
  where f s x = case (s, x) of
          (0, 'a') -> 1
          (0, 'b') -> -1
          (1, 'a') -> 2
          (1, 'b') -> -1
          (2, 'a') -> 3
          (2, 'b') -> -1
          (s, _) -> s

bbb = DFA "ab" f 0 (== 3)
  where f s x = case (s, x) of
          (0, 'a') -> 0
          (0, 'b') -> 1
          (1, 'a') -> 0
          (1, 'b') -> 2
          (2, 'a') -> 0
          (2, 'b') -> 3
          (3, 'a') -> -1
          (3, 'b') -> 3
          (-1, _) -> -1

same = DFA "ab" f "start" (`elem` ["a2","b2"])
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

evena = DFA "ab" f 0 (== 2)
  where f s x = case (s, x) of
          (0, 'a') -> 1
          (1, 'a') -> 2
          (2, 'a') -> 1
          (s,'b') -> s

------------------------------------------------------------

data DMA s i o = DMA [i] ([s] -> i -> [s]) ([s] -> o)

runDMA :: Eq i => DMA s i o -> [i] -> o
runDMA (DMA [] f post) = post . foldl f []
runDMA (DMA is f post) = post . foldl f [] . filter (`elem` is)

brackets = DMA [] f null
  where f ('(':s) ')' = s
        f ('[':s) ']' = s
        f ('{':s) ']' = s
        f s x | x `elem` "[]{}()" = x:s
              | otherwise = s

calcRPN = DMA [] f id
  where f (x:y:s) "+" = (x+y):s
        f (x:y:s) "-" = (y-x):s
        f (x:y:s) "*" = (x*y):s
        f (x:y:s) "/" = (y `div` x):s
        f s n = read n : s

------------------------------------------------------------
--process
--  :: Monoid p => (t -> a -> (t, p)) -> (t -> p) -> t -> [a] -> p
process step finish = go
  where go s [] = finish s
        go s (h:t) = res <> go s' t
          where (s', res) = step s h
                
data Token = N String | Op Char | Par Char
  deriving Show

tokenize :: String -> [Token]
tokenize s = process step pushNum [] s
  where step s x | isDigit x = (x:s, [])
                 | isOperator x = ([], pushNum s <> [Op x])
                 | x `elem` "()" = ([], pushNum s <> [Par x])
                 | otherwise = (s, [])
        pushNum s = if null s then [] else [N $ reverse s]

isOperator = (`elem` "+-*/")
isDigit = (`elem` "0123456789")        

------------------------------------------------------------
dijkstra :: String -> [String]
dijkstra = process step id [] . tokenize
  where step s x = case x of
          N n -> (s, [n])
          Op op -> pushOperator s [op]
          Par '(' -> ("(":s, [])
          Par ')' -> closePar s

        pushOperator s x = (x:s', o)
          where (o, s') = span (\y -> prec x < prec y) s

        closePar s = case span (/= "(") s of
          (_ ,[]) -> error "Unmatched parenthesis"
          (o, s') -> (tail s', o)

        prec x = case x of
          "*" -> 2
          "/" -> 2
          "+" -> 1
          "-" -> 1
          "(" -> 0

------------------------------------------------------------                                       
data M = M [[Double]] | I deriving Show

outer f l1 l2 = [[f x y | x <- l2 ] | y <- l1]
dot v1 v2 = sum $ zipWith (*) v1 v2

instance Semigroup M where
  m <> I = m
  I <> m = m
  M m1 <> M m2 = M $ outer dot m1 (transpose m2)

instance Monoid M where
  mempty = I

