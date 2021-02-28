{-# LANGUAGE BangPatterns #-}
module Lab4 where

import Data.Char
import Data.List
import Text.Printf

import Lab3

fromBase b = foldl (\r x -> r*b+x) 0  

data Alphabet i = Set [i]
                | Only (i -> Bool)
                | Any
                  
             
data Automat s i =
  Automat { alphabet :: Alphabet i -- допустимые символы
          , delta :: s -> i -> s -- функция перехода
          , start :: s -- начальное состояние
          , stop :: [s] -- останавливающие состояния
          , final :: [s] -- конечные состояния
          }

mod3 :: Automat Int Int 
mod3 = Automat (Set [0,1]) f 0 [] [0,1,2]
  where f 0 0 = 0
        f 0 1 = 1
        f 1 0 = 2
        f 1 1 = 0
        f 2 0 = 1
        f 2 1 = 2

scanA :: (Eq s, Eq i) => Automat s i -> [i] -> [(s, i)]
scanA m xs = takeWhile' (not . halt) $ zip states inputs
  where
    -- поток допустимых символов
    inputs = case alphabet m of
               Any -> xs
               Set alph -> takeWhile (`elem` alph) xs
               Only p -> takeWhile p xs
     -- поток состояний автомата
    states = tail $ scanl (delta m) (start m) inputs
     -- условие остановки работы
    halt = (`elem` stop m) . fst

takeWhile' p lst = case span p lst of
                     (a, []) ->  a
                     (a, b:_) -> a ++ [b]

           
ab_a = Automat (Set "ab") f 0 [] [3]
  where f s x = case (s, x) of
          (0, 'a') -> 1
          (0, 'b') -> -1
          (1, 'a') -> -1
          (1, 'b') -> 2
          (2, 'a') -> 3
          (2, 'b') -> 2
          (3, _) -> -1

abba = Automat (Set "ab") f 0 [4] [4]
  where f s x = case (s, x) of
          (0, 'a') -> 1
          (0, 'b') -> 0
          (1, 'a') -> 1
          (1, 'b') -> 2
          (2, 'a') -> 1
          (2, 'b') -> 3
          (3, 'a') -> 4
          (3, 'b') -> 0
                      
aba_ = Automat (Set "ab") f 0 [-1] [0]
  where f s x = case (s, x) of
          (0, 'a') -> 1
          (0, 'b') -> -1
          (1, 'a') -> -1
          (1, 'b') -> 2
          (2, 'a') -> 0
          (2, 'b') -> -1

_aaa = Automat (Set "ab") f 0 [] [3]
  where f s x = case (s, x) of
          (0, 'a') -> 1
          (0, 'b') -> 0
          (1, 'a') -> 2
          (1, 'b') -> 0
          (2, 'a') -> 3
          (2, 'b') -> 0
          (3, 'a') -> 3
          (3, 'b') -> 0

bbb_ = Automat (Set "ab") f 0 [-1,3] [3]
  where f s x = case (s, x) of
          (0, 'a') -> -1
          (0, 'b') -> 1
          (1, 'a') -> -1
          (1, 'b') -> 2
          (2, 'a') -> -1
          (2, 'b') -> 3

x_x = Automat (Set "ab") f "s" [] ["a","b"]
  where f s x = case (s, x) of
          ("s", 'a') -> "a"
          ("s", 'b') -> "b"
          ("a", 'a') -> "a"
          ("a", 'b') -> "wa"
          ("b", 'a') -> "wb"
          ("b", 'b') -> "b"
          ("wa", 'a') -> "a"
          ("wa", 'b') -> "wa"
          ("wb", 'a') -> "wb"
          ("wb", 'b') -> "b"


                      
runA :: (Eq s, Eq i) => Automat s i -> [i] -> s
runA m xs = case scanA m xs of
                [] -> start m
                res -> fst $ last res

testA :: (Eq s, Eq i) => Automat s i -> [i] -> Bool
testA m = (`elem` final m) . runA m

printA m xs = do putStrLn $ "\t" <> show (start m)
                 mapM_ f $ scanA m xs
  where f (x, s) = putStrLn $ show s ++ "\t" ++ show x

unfold f x = case f x of
  Nothing -> []
  Just (a, y) -> a : unfold f y

brackets = Automat Any f [] [] [[]]
  where f ('(' : s) ')' = s
        f ('[' : s) ']' = s
        f ('{' : s) '}' = s
        f s x | x `elem` "{}[]()" = x:s
        f s _ = s

calcRPN = Automat (Only operator_or_num) f [] [] []
  where f (x:y:s) "+" = (x+y):s
        f (x:y:s) "-" = (y-x):s
        f (x:y:s) "*" = (x*y):s
        f (x:y:s) "/" = (y `div` x):s
        f s n = read n : s

operator_or_num w = isOperator w || all (`elem` ['0'..'9']) w

isOperator w = w `elem` ["+","-","*","/"]

rpnToLisp = Automat Any f [] [] []
  where
    f (x:y:s) op | isOperator op = printf "(%s %s %s)" op y x : s
    f s n = n : s

            
-- data Token = S | N | O | P | E deriving (Show, Eq)
  
-- lexer = Automat (Set "01234567890+-*/()") f S [E] [O,P]
--   where f S x | x `elem` digits = N
--               | x `elem` "+-*/" = O
--               | x `elem` "()"   = P
--               | otherwise       = E
--         f N x | x `elem` digits = N
--               | otherwise       = E
--         f _     _               = E
        
--         digits = "0123456789"

-- prefixA m xs = case scanA m xs of
--   [] -> Nothing
--   r -> Just ((fst <$> r, snd (last r)), drop (length r) xs)

-- tokenize = unfold (prefixA lexer) . filter (`elem` alphabet lexer)

------------------------------------------------------------

md = 101
  
hashStep :: Int -> Char -> Int
hashStep s ch = (s * 256 + ord ch) `mod` md

hash :: String -> Int
hash = foldl hashStep 0 


hashAdd :: Int -> Char -> Char -> Int -> Int
hashAdd n a b s = hashStep (s - ord a*pow 256 n md) b

pow :: Int -> Int -> Int -> Int
pow a b m = foldl (\r x -> (r*x) `mod` m) a $ replicate (b-1) a

--findSub :: String -> String -> [Int]
--findSub [sub] s = elemIndices sub s
findSub sub s   = map (\(i,_,_) -> i) $
                  filter test $
                  zip3 [0..] (tails s) $
                  scanl' hashAdd (hash prefix) $
                  zip s s'
  where
    n = length sub
    (prefix, s') = splitAt n s
    hashAdd s (!a, !b) = hashStep (s - ord a*shift) b
    shift = pow 256 (n-1) md
    hs = hash sub
    test (_, s', h) = h == hs && take n s' == sub

nums = concat $ show <$> [1..]

subs :: String -> String -> [Int]
subs sub s = elemIndices sub $ take (length sub) <$> tails s

floyd = zipWith (\s i -> [s .. s+i])
        (tail (scanl (+) 1 [0 ..]))
        [0 ..]

data Flag = InsideWord | Spaces deriving (Show, Eq)
          
wc = Automat Any f (0, Spaces) [] []
    where f (s, InsideWord) ' ' = (s, Spaces)
          f (s, InsideWord) _ = (s, InsideWord)
          f (s, Spaces) ' ' = (s, Spaces)
          f (s, Spaces) _ = (s+1, InsideWord)
