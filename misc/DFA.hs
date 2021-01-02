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

dijkstra = DMA [] f fst
  where f (s, o) x
          | operator x = pushOperator x (s, o)
          | digit x = pushDigit x (s, o)
          | x == "(" = ("(" : s, o)
          | otherwise = (s, o)

        pushDigit d (s, []) = (s, [d])
        pushDigit d (s, n:o) = (s, (d:n):o)
        
        pushOperator x ([], o) = ([x], o)
        pushOperator x (y:s, o)
          | prec x > prec y = (x:y:s, o)
          | otherwise = pushOperator x (s, y:o)

        prec x = case x of
          "+" -> 1
          "-" -> 1
          "*" -> 2
          "/" -> 2
                                       
