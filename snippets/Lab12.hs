module Lab12 where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import Data.Maybe
import Data.List
import Lab4
import Lab8 (calc, BTree (..), traditional)

newtype Logic a = From { toList :: [a] }

instance Show a => Show (Logic a) where
  show (From l) = "Logic " <> case l of
                                [] -> "[]"
                                [x] -> show x
                                x:_ -> show x <> " ..."
headL :: Logic a -> Maybe a 
headL = listToMaybe . toList

takeL :: Int -> Logic a -> [a]
takeL n = take n . toList

interleave l1 l2 = case l1 of
  [] -> l2
  x:xs -> x : interleave l2 xs

instance Functor Logic where
  fmap f (From lst) = From $ fmap f lst

instance Applicative Logic where
  pure x = From [x]
  (<*>) = ap

instance Semigroup (Logic a) where
  From l1 <> From l2  = From $ l1 <> l2

instance Monoid (Logic a) where
  mempty = empty

instance Alternative Logic where
  empty = From []
  From l1 <|> From l2  = From $ interleave l1 l2

instance Monad Logic where
  From [] >>= _ = empty
  From (x:xs) >>= f = f x <|> (From xs >>= f)

instance MonadFail Logic where
  fail _ = empty

different :: (Eq a) => Int -> [a] -> Logic [a]
different 0 _ = pure []
different n lst = do x <- From lst
                     xs <- different (n-1) $ filter (/= x) lst
                     pure (x:xs)

samples 0 _ = pure []
samples n lst = (:) <$> From lst <*> samples (n-1) lst

fact 0 x = x == 1
fact n m = m `mod` n == 0 && fact (n-1) (m `div` n)


prob1 = do [a,p,k] <- different 3 [1,2,3]
           guard $ and [ a /= 2
                       , p > a
                       , k /= 3 ]
           pure (a,p,k)


queens :: Int -> Logic [Int]
queens n = go n
    where
      go 0 = pure []
      go k = do qs <- go (k-1)
                q <- From [1..n]
                guard . not $ q `elem` qs
                guard . not $ sameDiag q qs
                pure (q:qs)
      sameDiag q = any (\(c, x) -> abs (q - x) == c) . zip [1..]

showQueens n = foldMap line
  where
    line i = replicate (i-1) '.'
             <> "Q"
             <> replicate (n-i+1) '.'
             <> "\n"

------------------------------------------------------------
--  SEND
--  MORE
-- MONEY

prob2 = do
  [s,e,n,d,m,o,r,y] <- different 8 [0..9]
  guard $ s*m /= 0
  let a =   fromBase 10 [s,e,n,d]
      b =   fromBase 10 [m,o,r,e]
      c = fromBase 10 [m,o,n,e,y]
  guard $ a + b == c
  return (a,b,c)

prob2' = do
  [d,e,y] <- different 3 [0..9]
  guard $ (d + e) `mod` 10 == y
  [n,r] <- different 2 $ [0..9] \\ [d,e,y]
  guard $ (n + r + (d + e) `div` 10) `mod` 10 == e
  o <- From $ [0..9] \\ [d,e,y,n,r]
  guard $ (e + o + (n + r) `div` 10) `mod` 10 == n
  [s,m] <- different 2 $ [1..9] \\ [d,e,y,n,r]
  guard $ (s + m + (e + o) `div` 10) `mod` 10 == o
  let a =   fromBase 10 [s,e,n,d]
      b =   fromBase 10 [m,o,r,e]
      c = fromBase 10 [m,o,n,e,y]
  guard $ a + b == c
  return (a,b,c)

------------------------------------------------------------

prob3 m n = do
  [o1,o2,o3] <- samples 3 ["+","-","*","/"]
  let s = Leaf (show m)
  expr <- From [ Node o1 (Node o2 s s)
                         (Node o3 s s)
               , Node o1 s (Node o2 s (Node o3 s s))
               , Node o1 (Node o2 (Node o3 s s) s) s
               , Node o1 (Node o2 s (Node o3 s s)) s
               , Node o1 s (Node o2 (Node o3 s s) s) ]
  guard $ calc expr == n
  return $ traditional expr <> " = " <> show n

trees 1 = [Leaf ()]
trees 2 = [Node () (Leaf ()) (Leaf ())]
trees n = nub $ trees (n-1) >>= ins (Node () (Leaf ()) (Leaf ()))
  where
    ins t (Leaf ()) = [t]
    ins t (Node () t1 t2)
      = (Node () <$> ins t t1 <*> [t2]) <|> (Node () <$> [t1] <*> ins t t2)

------------------------------------------------------------

unfoldM :: (Alternative m, Monad m)
        => (a -> m (b, a)) -> a -> m [b]
unfoldM f a = do (b, c) <- f a
                 bs <- unfoldM f c <|> pure []
                 return $ b : bs

wordify :: [String] -> String -> [] [String]
wordify d s = unfoldM (foldMap prefix d) s
  where
    prefix p = go p
      where
        go [] str = pure (p, str)
        go (x:xs) (y:ys) | x == y = go xs ys
        go _ _ = empty


dict = words "and cream go i ice icecream like man mango mobile sam samsung sung"
s = "ilikeicecreammangoandsamsung"

------------------------------------------------------------


type Space s m = [(m , s)]
type Move s m = s -> Space s m
type Strategy s m = Space s m -> Space s m -> Space s m

space :: Monoid m =>
  Strategy s m -> Move s m -> s -> Space s m
space f trans s = expand f (step (mempty, s))
    where expand f [] = mempty
          expand f (s:ss) = s:expand f (f (step s) ss)
          step (ms, s) = [(ms <> m, t) | (m, t) <- trans s]

search :: Monoid m =>
  Strategy s m -> Move s m -> ((m, s) -> Bool) -> s -> Space s m
search f trans isSolution = filter isSolution . space f trans

dfs, bfs :: Monoid m => Strategy s m
dfs = (<>)
bfs = flip (<>)

------------------------------------------------------------

wordSplit dict s = unwords.fst <$> search dfs trans isSolution s
  where
    trans = foldMap prefix dict
    isSolution (_, s) = null s
    prefix p s = go p s
      where
        go [] str = [([p], str)]
        go (x:xs) (y:ys) | x == y = go xs ys
        go _ _ = mempty

------------------------------------------------------------

isqrt n | fromIntegral rs == s = pure rs
        | otherwise = empty
  where s = sqrt (fromIntegral n)
        rs = round s

triangles = do
  a <- From [1..]
  b <- From [1..a]
  guard $ gcd a b == 1
  c <- isqrt (a^2 + b^2)
  return (b,a,c)

area (a,b,_) = a * b `div`2

perimiter (a,b,c) = a + b + c
