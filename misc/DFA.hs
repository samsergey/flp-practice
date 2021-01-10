data Automat s i =
  Automat { alphabet :: [i] -- допустимые символы
          , delta :: s -> i -> s -- функция перехода
          , start :: s -- начальное состояние
          , stop :: [s] -- останавливающие состояния
          , final :: [s] -- конечные состояния
          }

mod3 :: Automat Int Int 
mod3 = Automat [0,1] f 0 [] [0,1,2]
  where f 0 0 = 0
        f 0 1 = 1
        f 1 0 = 2
        f 1 1 = 0
        f 2 0 = 1
        f 2 1 = 2

scanA :: (Eq s, Eq i) => Automat s i -> [i] -> [(i, s)]
scanA m xs = takeWhile (not . halt . snd) 
               $ zip inputs states
  where
    -- поток допустимых символов
    inputs = case alphabet m of
               [] -> xs
               alph -> takeWhile (`elem` alph) xs
     -- поток состояний автомата
    states = tail $ scanl (delta m) (start m) inputs
     -- условие остановки работы
    halt = (`elem` stop m)

abba = Automat "ab" f 0 [-1] [4]
  where f s x = case (s, x) of
          (0, 'a') -> 1
          (0, 'b') -> -1
          (1, 'a') -> -1
          (1, 'b') -> 2
          (2, 'a') -> 4
          (2, 'b') -> 2
          (4, _) -> 4

runA :: (Eq s, Eq i) => Automat s i -> [i] -> s
runA m xs = case scanA m xs of
                [] -> start m
                res -> snd $ last res

testA :: (Eq s, Eq i) => Automat s i -> [i] -> Bool
testA m = (`elem` final m) . runA m

printA m = mapM_ f . scanA m
  where f (x, s) = putStrLn $ show x ++ "\t" ++ show s 

prefixA m xs = case scanA m xs of
  [] -> Nothing
  res -> Just ((snd (last res), fst <$> res), drop (length res) xs)


brackets = Automat "()[]{}" f [] [] [[]]
  where f ('(' : s) ')' = s
        f s x = x:s

calcRPN = Automat [] f [] [] []
  where f (x:y:s) "+" = (x+y):s
        f (x:y:s) "-" = (y-x):s
        f (x:y:s) "*" = (x*y):s
        f (x:y:s) "/" = (y `div` x):s
        f s n = read n : s

data Token = S | N | O | P | E deriving (Show, Eq)
  
lexer = Automat "0123456789+-*/()" f S [E] [O,P]
  where f S x | x `elem` digits = N
              | x `elem` "+-*/" = O
              | x `elem` "()"   = P
              | otherwise       = E
        f N x | x `elem` digits = N
              | otherwise       = E
        f s _                   = E
        
        digits = "0123456789"

