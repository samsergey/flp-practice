{-# LANGUAGE FlexibleContexts, DeriveFunctor,FlexibleInstances,TypeSynonymInstances #-}

module Lab8 where

import Control.Applicative hiding (some,many)
import Control.Monad hiding (fail)
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Monoid
import System.CPUTime
import Text.Printf
import Logic
import Lab3 (Tree (..))
import Lab5 ((*<>))
import Lab7 

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
  

mkTree :: Int -> State Int (Tree Int)
mkTree 0 = leaf <$> modify (+ 1)
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

enumTree :: Int -> Tree Int
enumTree n = snd $ runState (mkTree n) 0


type Random a = State Integer a

random :: Integral a => a -> Random a
random k = rescale <$> modify iter 
  where
    iter x = (x * a + c) `mod` m
    (a, c, m) = (1103515245, 12345, 2^31-1)
    rescale x = fromIntegral x `mod` k

randomTree :: Int -> Random (Tree Int)
randomTree 0 = leaf <$> random 100
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
peek = take 1 <$> get

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
dijkstra s = evalStackFn $ do
   res <- foldMapM step s
   s <- get
   return $ res <> s
  
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

{-
calcIO :: IO ()
calcIO = do s <- getLine
            case calculateA $ toRPN s of
              Right [] -> calcIO
              Right r -> putStr "> " >> print (head r)
              Left e -> print $ "Error in operation " <> e
            calcIO
-}

floyd :: [[Int]]
floyd = mapM (`replicateM` (modify succ)) [1..] `evalState` 0

leaf x = Node x Empty Empty
        
traditional (Node a Empty Empty) = a
traditional (Node a t1 t2) = printf "(%s%s%s)" (traditional t1) a (traditional t2)

lisp (Node a Empty Empty) = a
lisp (Node a t1 t2) = printf "(%s %s %s)"  a (lisp t1) (lisp t2)

rpn' (Node a Empty Empty) = a
rpn' (Node a t1 t2) = printf "%s %s %s"  (rpn' t1) (rpn' t2) a

calc (Node a Empty Empty) = read a
calc (Node a t1 t2) = case a of
  "+" -> calc t1 + calc t2
  "-" -> calc t1 - calc t2
  "*" -> calc t1 * calc t2
  "/" -> calc t1 / calc t2
  "^" -> calc t1 ** calc t2

randomAST :: Int -> Random (Tree String)
randomAST 0 = leaf . show <$> random 10
randomAST n = Node <$> randomSample ["+","-","*","/"]
               <*> (random n >>= randomAST)
               <*> (random n >>= randomAST)

randomSample :: [a] -> Random a
randomSample lst = (cycle lst !!) <$> random 10000

------------------------------------------------------------
chA :: a -> Grammar (Random a)
chA = pure . pure

altA, strA :: [a] -> Grammar (Random a)
altA = getAlt . foldMap (Alt . chA)
strA xs = foldMap chA xs

arythmeticsA :: Grammar (Random Char)
arythmeticsA = expr
  where
    expr = term <> many (altA "+-" <> term)
    term = mult <> many (altA "*/" <> mult)
    mult = num <|> chA '(' <> expr <> chA ')'
    num = rnd "123456789"

rnd = pure . randomSample 

powers x = scanl (<>) mempty $ repeat x

languageA :: Grammar (Random a) -> Random [[a]]
languageA = fmap samples . traverse sequenceA . generate

rep :: (Int, Int) -> Grammar a -> Grammar a
rep (n, m) x = asum $ (*<> x) <$> [n..m]
              
emails = login <> chA '@' <> dom <> rep (1,2) (chA '.' <> dom)
  where
    login = rep (4,8) $ rnd (alnum <> ".")
    dom = rep (2,3) $ rnd ['a'..'z']
    alnum = 3 *<> (['0'..'9'] <> ['a'..'z']) <> "_-" <> ['A'..'Z']

          

------------------------------------------------------------

-- generate :: Alternative f => Grammar a -> f [a]
-- generate g = case g of
--                None -> empty
--                Kleene None -> empty
--                Epsilon -> pure []
--                Kleene Epsilon -> pure []
--                Term c -> pure [c]
-- --               Anything -> generate $ alt $ alphabeth g
--                Kleene x -> generate $ opt (some x)
--                Alter a b -> generate a <|> generate b
--                Chain a b -> (++) <$> generate a <*> generate b

instance Alternative (State Integer) where
    empty = undefined
    a <|> b = do i <- random 2
                 if i < 1 then a else b 

generateR :: Grammar Char -> Random String
generateR g = case g of
               None -> pure []
               Kleene None -> pure []
               Epsilon -> pure []
               Kleene Epsilon -> pure []
               Term c -> pure [c]
               Kleene a -> chose $ take 10 $ powers a
               Alter a None -> generateR a
               Alter a b -> generateR a <|> generateR b
               Chain a b -> (++) <$> generateR a <*> generateR b
    where
      chose lst = mapM generateR lst >>= randomSample

rept 0 f = id
rept 1 f = f
rept n f = f . rept (n-1) f

      
