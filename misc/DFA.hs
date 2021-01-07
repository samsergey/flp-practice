import Data.List (transpose, unfoldr)

data FSM s i = FSM { inpust :: [i]
                   , delta :: s -> i -> s
                   , start :: s
                   , stop :: [s] }
  
runFSM :: Eq i => FSM s i -> [i] -> s
runFSM (FSM is f s0 _) = foldl f s0 . filter (`elem` is)

testFSM :: (Eq s, Eq i) => FSM s i -> [i] -> Bool
testFSM fsm = (`elem` stop fsm) . runFSM fsm

scanFSM :: Eq i => FSM s i -> [i] -> [s]
scanFSM (FSM is f s0 _) = scanl f s0 . filter (`elem` is)

mod3 :: FSM Int Int 
mod3 = FSM [0,1] f 0 [0,1,2]
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

abba = FSM "ab" f 0 [4]
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

aaa = FSM "ab" f 0 [4]
  where f s x = case (s, x) of
          (0, 'a') -> 1
          (0, 'b') -> -1
          (1, 'a') -> 2
          (1, 'b') -> -1
          (2, 'a') -> 3
          (2, 'b') -> -1
          (3, _) -> 4
          (-1,_) -> -1

bbb =  FSM "ab" f 0 [3]
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

same = FSM "ab" f "start" ["a2","b2"]
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


evena = FSM "ab" f 0 [2]
  where f s x = case (s, x) of
          (0, 'a') -> 1
          (1, 'a') -> 2
          (2, 'a') -> 1
          (s,'b') -> s


lexer = FSM "0123456789+-*/()" f 0 [-1]
  where f 0 x | isDigit x = 1
              | isOperator x = 2
              | isParenthesis x = 3
              | otherwise = -2
        f 1 x | isDigit x = 1
              | otherwise = -1
        f s _ = -1

prefixFSM (FSM is f s0 stop) = go s0 []
  where go s xs []
          | s == s0 = Nothing
          | otherwise = Just ((s, xs), [])
        go s xs (h:t)
          | s' `elem` stop = Just ((s, xs), h:t)
          | otherwise = go s' (xs++[h]) t
          where s' = f s h


------------------------------------------------------------

runPA :: ([s] -> i -> [s]) -> [i] -> [s]
runPA f = foldl f []

brackets = null . runPA f 
  where f ('(':s) ')' = s
        f ('[':s) ']' = s
        f ('{':s) ']' = s
        f s x | x `elem` "[]{}()" = x:s
              | otherwise = s

calcRPN = runPA f
  where f (x:y:s) "+" = (x+y):s
        f (x:y:s) "-" = (y-x):s
        f (x:y:s) "*" = (x*y):s
        f (x:y:s) "/" = (y `div` x):s
        f s n = read n : s

------------------------------------------------------------

runPT :: ([s] -> i -> ([s], [o]))
      -> ([s] -> [o])
      -> [i] -> [o]
runPT step finish = go []
  where go s [] = finish s
        go s (h:t) = let (s', res) = step s h
                     in res ++ go s' t

------------------------------------------------------------
                        
data Token = N String | Op String | Par String
  deriving Show

tokenize :: String -> [Token]
tokenize = runPT step pushNum 
  where
    step s x
      | isDigit x = (x:s, [])
      | isOperator x = ([], pushNum s <> [Op [x]])
      | x `elem` "()" = ([], pushNum s <> [Par [x]])
      | otherwise = (s, [])
    pushNum s = if null s then [] else [N $ reverse s]

isOperator = (`elem` "+-*/")
isParenthesis = (`elem` "()")
isDigit = (`elem` "0123456789")        

------------------------------------------------------------
dijkstra :: String -> [String]
dijkstra = runPT step id . tokenize
  where step s x = case x of
          N n -> (s, [n])
          Op op -> pushOperator s op
          Par "(" -> ("(":s, [])
          Par ")" -> closePar s

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

