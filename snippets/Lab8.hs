{-# LANGUAGE FlexibleInstances #-}
module Lab8 where

import Lab7 (headE, calculateA)

import Control.Applicative
import Control.Monad
import Control.Monad.Logic

import System.CPUTime
import Text.Printf

data State s a = State { runState :: s -> (s, a) }

evalState st = snd . runState st

instance Functor (State s) where
  fmap f (State p) = State $ \s -> f <$> p s

instance Applicative (State s) where
  pure x  = State $ \s -> (s, x)
  (<*>) = ap
  
instance Monad (State s) where
  State p >>= f = State $ \s -> let ~(s', y) = p s
                                in runState (f y) s'
get :: State s s
get = State $ \s -> (s, s)

set :: s -> State s s
set s = State $ const (s, s)

modify :: (s -> s) -> State s s
modify f = get >>= set . f
  
data BTree a = Leaf a
             | Node a (BTree a) (BTree a)
  deriving Show

mkTree :: Int -> State Int (BTree Int)
mkTree 0 = Leaf <$> modify (+ 1)
-- enumTree n = Node <$> modify (+ 1)
--              <*> enumTree (n-1)
--              <*> enumTree (n-1)

-- enumTree n =  modify (+ 1) >>= \i
--               -> enumTree (n-1) >>= \l
--               -> enumTree (n-1) >>= \r
--               -> pure (Node i l r)

mkTree n = do l <- mkTree (n-1)
              i <- modify (+ 1)
              r <- mkTree (n-1)
              return $ Node i l r

enumTree :: Int -> BTree Int
enumTree n = snd $ runState (mkTree n) 0


type Random a = State Integer a

random :: Integral a => a -> Random a
random k = rescale <$> modify iter 
  where
    iter x = (x * a + c) `mod` m
    (a, c, m) = (1103515245, 12345, 2^31-1)
    rescale x = fromIntegral x `mod` k

randomTree :: Int -> Random (BTree Int)
randomTree 0 = Leaf <$> random 100
randomTree n = Node <$> random 100
               <*> (random n >>= randomTree)
               <*> (random n >>= randomTree)

heart :: Double -> Double -> Bool
heart x y = (x**2+y**2-1)**3 < x**2 * y**3

randomDouble :: Random Double
randomDouble = (\r -> 1e-8 * fromIntegral r) <$> random (10^8)

rndIn (a,b) = (\r -> a+r*(b-a)) <$> randomDouble 

area :: (Double -> Double -> Bool)
   -> (Double, Double)
   -> (Double, Double)
   -> Random Double
area f (x1,x2) (y1,y2) = do
  let fn = f <$> rndIn (x1,x2) <*> rndIn (y1,y2)
  pts <- replicateM ntrials fn
  let n = fromIntegral . length $ filter id pts
  return $ (n / fromIntegral ntrials)*(x2-x1)*(y2-y1)
  where
    ntrials = 100000

------------------------------------------------------------
type StackFn a = State [a] [a]

evalStackFn :: StackFn a -> [a]
evalStackFn fn = evalState fn []

-- поместить значение на вершину стека
push :: a -> StackFn a
push x = modify (x:) >> return []

-- получить значение на вершине стека
peek :: StackFn a
peek = headE <$> get

-- снять значение с вершины стека
pop :: StackFn a
pop = get >>= split
  where split [] = return []
        split (x:s) = set s >> return [x]

-- снимать с вершины стека все значения
-- до тех пор, пока выполняется указанное условие
popWhile :: (a -> Bool) -> StackFn a
popWhile p = do (r, s') <- span p <$> get
                set s'
                return r

foldMapM f xs = mconcat <$> mapM f xs 

step :: String -> StackFn String
step x
  | isOperator x =
      do r <- popWhile (\y -> prec x < prec y)
         push x
         return r
  | x == "(" = push "("
  | x == ")" =
      do r <- popWhile (/= "(")
         pop
         return r
  | otherwise = return [x]

isOperator = (`elem` ["+","-","*","/"])

prec x = case x of
  "*" -> 2
  "/" -> 2
  "+" -> 1
  "-" -> 1
  "(" -> 0

toRPN = unwords . dijkstra . lexer

dijkstra :: [String] -> [String]
dijkstra s = evalStackFn $ (<>) <$> foldMapM step s <*> get
  
lexer :: String -> [String]
lexer = words . foldMap separate
  where separate x
          | x `elem` "+-*/()" = " " <> [x] <> " "
          | otherwise = [x]

randomIO :: Integral a => a -> IO a
randomIO n = evalState (random n) <$> getCPUTime

ugadaika :: Int -> IO ()
ugadaika n = randomIO n >>= dialog

dialog :: Int -> IO ()
dialog x = next
  where next = do
          y <- read <$> getLine
          case compare x y of
            LT -> print "less" >> next
            GT -> print "greater" >> next
            EQ -> print "yes!"

calcIO :: IO ()
calcIO = do s <- getLine
            case calculateA $ toRPN s of
              Right [] -> calcIO
              Right r -> putStr "> " >> print (head r)
              Left e -> print $ "Error in operation " <> e
            calcIO

floyd :: [[Int]]
floyd = mapM (`replicateM` (modify succ)) [1..] `evalState` 0

traditional (Leaf a) = a
traditional (Node a t1 t2) = printf "(%s %s %s)" (traditional t1) a (traditional t2)

lisp (Leaf a) = a
lisp (Node a t1 t2) = printf "(%s %s %s)"  a (lisp t1) (lisp t2)

rpn (Leaf a) = a
rpn (Node a t1 t2) = printf "%s %s %s"  (rpn t1) (rpn t2) a

calc (Leaf a) = read a
calc (Node a t1 t2) = case a of
  "+" -> calc t1 + calc t2
  "-" -> calc t1 - calc t2
  "*" -> calc t1 * calc t2
  "/" -> calc t1 / calc t2

randomAST :: Int -> Random (BTree String)
randomAST 0 = Leaf . show <$> random 10
randomAST n = Node <$> randomSample ["+","-","*","/"]
               <*> (random n >>= randomAST)
               <*> (random n >>= randomAST)

randomSample lst = (lst !!) <$> random (length lst)


