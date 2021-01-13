{-# LANGUAGE DeriveFunctor #-}
{-# language FlexibleInstances #-}

import Control.Applicative
import Control.Monad
import System.CPUTime

type Stack = [Double]

calculate :: String -> Stack
calculate = foldl interprete [] . words
  where
    interprete s op = case op of
      "+" -> binary (+)
      "*" -> binary (*)
      "-" -> binary (-)
      "/" -> binary (/)
      "sqrt" -> case s of
        x:s | x >= 0  -> sqrt x : s
            | otherwise  -> err "negative argument!"
        [] -> err "got no arguments!"
      n -> read n : s
      where
        binary f = case s of
          x:y:s -> f y x : s
          [_]   -> err "got only one argument!"
          []    -> err "got no arguments!"

        err m = error $ op ++": " ++ m
        
------------------------------------------------------------

readE :: Read a => String -> Either String a
readE s = case [x | (x,t) <- reads s, ("","") <- lex t] of
            [x] -> Right x
            _ -> Left $ "could not parse " ++ s

calculateE :: String -> Either String Stack
calculateE = foldM interprete [] . words
  where
    interprete s op = case op of
      "+" -> binary (+)
      "*" -> binary (*)
      "-" -> binary (-)
      "/" -> binary (/)
      "sqrt" -> case s of
        x:s | x >= 0  -> Right $ sqrt x : s
            | otherwise  -> err "negative argument!"
        [] -> err "got no arguments!"
      n -> Right $ read n : s
      where
        binary f = case s of
          x:y:s -> Right $ f y x : s
          [_]   -> err "got only one argument!"
          []    -> err "got no arguments!"

        err m = Left $ op ++": " ++ m
        

------------------------------------------------------------
class (Alternative m, Monad m) => Exception m where
  exception :: String -> m a

instance Exception Maybe where
  exception = const Nothing

instance Exception [] where
  exception = const []

instance Exception IO where
  exception = error

type Err = Either String

instance Exception Err where
  exception = Left

instance Alternative Err where
  empty = exception ""
  Right r <|> _ = Right r
  Left _  <|> Right r = Right r
  Left l  <|> Left _ = Left l

readS :: (Exception ex, Read a) => String -> ex a
readS s = case [x | (x,t) <- reads s, ("","") <- lex t] of
            [x] -> pure x
            _ -> exception $ "could not parse " ++ s


calculateA :: Exception ex => String -> ex Stack
calculateA = foldM interprete [] . words
  where
    interprete s op = case op of
      "+" -> binary (+)
      "*" -> binary (*)
      "-" -> binary (-)
      "/" -> binary (/)
      "sqrt" -> case s of
        x:s | x > 0  -> pure (sqrt x:s) <|> pure ((- sqrt x):s)
            | x == 0 -> pure (0:s)
            | x < 0  -> err "negative argument!"
        []           -> err "got no arguments!"
      "pm" -> case s of
        x:y:s -> pure (x+y : s) <|> pure (y-x : s)
        _ -> empty
      n -> case readS n of
        Just x  -> pure $ x : s
        Nothing -> err "unknown symbol!"
      where
        binary f = case s of
          x:y:s -> pure $ f y x : s
          [_]   -> err "got only one argument!"
          []    -> err "got no arguments!"

        err m = exception $ op ++": " ++ m ++ "  Stack: " ++ show s
        
--foldM f x = foldl (\res el -> res >>= (`f` el)) (pure x)

------------------------------------------------------------
      
data Env ex e a = Env {runEnv :: e -> ex a}

instance Functor ex => Functor (Env ex e) where
  fmap f (Env st) = Env $ fmap (fmap f) st

instance Exception ex => Applicative (Env ex e) where
  pure x = Env $ \e -> pure x
  (<*>)= ap

instance Exception ex => Alternative (Env ex e) where
  empty = exception ""
  m1 <|> m2 = Env $ \e -> runEnv m1 e <|> runEnv m2 e

instance Exception ex => Monad (Env ex e) where
  x >>= f = Env $ \e -> do y <- runEnv x e
                           runEnv (f y) e

instance Exception ex => Exception (Env ex e) where
  exception m = Env $ \e -> exception m


env :: Exception ex => Env ex e e
env = Env $ \e -> pure e

type Dict = [(String, Double)]

calculateD :: Exception ex => String -> Env ex Dict Stack
calculateD = foldM interprete [] . words
  where
    interprete s op = case op of
      "+" -> binary (+)
      "*" -> binary (*)
      "-" -> binary (-)
      "/" -> binary (/)
      n   -> env >>= \d -> case readS n <|> lookup n d of
        Just x -> pure $ x : s
        Nothing -> err "unknown command!"
      where
        binary f = case s of
          x:y:s -> pure $ f y x : s
          [_]   -> err "got only one argument"
          []    -> err "got no arguments"

        err m = exception $ op ++": " ++ m ++ "  Stack: " ++ show s

------------------------------------------------------------

data State s a = State { runState :: s -> (s, a) }

evalState st = snd . runState st

instance Functor (State s) where
  fmap f x = State $
             \s -> let (s', y) = runState x s
                   in (s', f y)

instance Applicative (State s) where
  pure x  = State $ \s -> (s, x)
  x <*> y = State $ \s -> let (s', f) = runState x s
                          in f <$> runState y s'

instance Monad (State s) where
  x >>= f = State $ \s -> let (s', y) = runState x s
                          in runState (f y) s'

get = State $ \s -> (s, s)
set x = State $ \_ -> (x, x)
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

random :: Num a => a -> Random a
random k = rescale <$> modify iter 
  where
    iter x = (x * a + c) `mod` m
    (a, c, m) = (1103515245, 12345, 2^31)
    rescale x = k * fromIntegral (x `div` m)

    
randomTree 0 = Leaf <$> random 100
randomTree n = Node <$> random 100
               <*> (random n >>= randomTree)
               <*> (random n >>= randomTree)

randomTreeIO n = evalState (randomTree n) <$> getCPUTime

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

-- push :: a -> State [a] []
-- push x = modify (x:) >> return []

-- peek :: State [a] [a]
-- peek = do s <- get
--           case s of
--             [] -> []
--             x:s -> return [x]

-- pop :: State [a] [a]
-- pop = do x <- peek
--          modify tail
--          return [x]

-- popWhile p = peek >>= \x -> if p x then pop else popWhile p


-- dijkstra s = go -- evalState (mapM go s) []
--   where
--     go x | isOperator x = popWhile (\y -> prec x < prec y) 
--          | x == "("     = push '('
--          | x == ")"     = popWhile (/= '(')
--          | otherwise    = return x

--     isOperator = (`elem` ["+","-","*","/"])

--     prec x = case x of
--       "+" -> 1
--       "-" -> 1
--       "*" -> 2
--       "/" -> 2
--       "(" -> 0
  


