data DFA s i o = DFA
  { symbols :: [i]
  , func :: s -> i -> s
  , start :: s
  , result :: s -> o }

runDFA :: Eq i => DFA s i o -> [i] -> o
runDFA (DFA is f s0 post) = post . foldl f s0 . filter (`elem` is)

mod3 = DFA [0,1] f 0 id
  where f s x = case (s,x) of
                  (0, 0) -> 0
                  (0, 1) -> 1
                  (1, 0) -> 2
                  (1, 1) -> 0
                  (2, 0) -> 1
                  (2, 1) -> 2

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
