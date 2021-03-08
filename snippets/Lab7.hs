{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Lab7 where

import Lab1 (mean)
import Lab4
import Lab5 ((*<>))
import Control.Applicative hiding (many, some)
import Control.Monad (foldM, ap)
import Logic
import Data.List
import Data.Ord
import Data.Maybe
import Data.Monoid
import Parsing (run, mmany, check, next, Result (..)) 
    
unless test p = if test then p else empty
untill test x p = if test then pure x else p

bisection p (a,b) | p a == p b = empty
                  | (b-a) < 1e-11 = pure c
                  | otherwise =
                      bisection p (a,c) <|> bisection p (c,b)
  where c = mean a b
       
        
type Stack = [Double]

{-
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

readE :: (Exception ex, Read a) => String -> ex a
readE s = case [x | (x,t) <- reads s, ("","") <- lex t] of
            [x] -> pure x
            _ -> exception $ "could not parse " ++ s

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

headE [] = exception "List is empty!"
headE (x:_) = pure x

tailE [] = exception "List is empty!"
tailE (_:s) = pure s

        
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

spaces' s = case s of
  x:xs -> (x :) <$> go xs
  "" -> [""]
  where
    go s = case s of
      x:xs -> (++) <$> [[x], [' ',x]] <*> go xs
      "" -> [""]
-}
------------------------------------------------------------
                   
data Grammar a =
  Epsilon
  | None
--  | Anything
  | Term a                        -- литерал
  | Kleene (Grammar a)            -- звезда Клини (повторение)
  | Alter (Grammar a) (Grammar a) -- объединение (альтернатива)
  | Chain (Grammar a) (Grammar a) -- цепочка (конкатенация)
    deriving (Functor, Eq, Show)
             
instance Monoid (Grammar a) where
  mempty = Epsilon

instance Semigroup (Grammar a) where
  (<>) = Chain

instance Alternative Grammar where
  empty = None
  (<|>) = Alter
  
instance Applicative Grammar where
  pure = Term
  f <*> x = case f of 
     Epsilon   -> Epsilon
     None      -> None
--     Anything  -> Anything
     Term f    -> f <$> x
     Kleene f  -> Kleene (f <*> x)
     Alter f g -> Alter (f <*> x) (g <*> x)
     Chain f g -> Chain (f <*> x) (g <*> x)

-- instance Show (Grammar Char) where
--   show = go . simplify
--     where go g = case g of
--             Epsilon -> ""
--             None -> "#"
--             Anything -> "."
--             Term a -> unless (meta a) "\\" <> pure a
--             Kleene g -> group g <> "*"
--             Alter a Epsilon -> go a <> "?"
--             Alter Epsilon a -> group a <> "?"
--             Alter a b -> "(" <> go a <> "|" <> go b <> ")"
--             Chain a b -> go a <> go b

--           group g = case g of
--               Term _ -> go g
--               Alter _ _ -> go g
--               _ -> "(" <> go g <> ")"

--           meta = (`elem` ".|*+?()[]#")

-- instance Show (Grammar Char -> Grammar Char) where
--   show g = show $ simplify $ g $ str "<self>"

          
ch :: a -> Grammar a
ch x = pure x
str s = foldMap pure s
alt x = getAlt $ foldMap pure x

opt g = Epsilon <|> g
many g = Kleene g
some g = g <> Kleene g

oneof p = getAlt . foldMap (Alt . p)

charset = ['a'..'z'] ++ [' '..'`']

alphabeth :: Eq a => Grammar a -> [a]
alphabeth g = case g of
   Epsilon -> empty
   None -> empty
   Term a -> pure a
--   Anything -> charset
   Kleene a -> alphabeth a
   Alter a b -> alphabeth a `union` alphabeth b
   Chain a b -> alphabeth a `union` alphabeth b

        
generate :: Alternative f => Grammar a -> f [a]
generate g = case g of
               None -> empty
               Kleene None -> empty
               Epsilon -> pure []
               Kleene Epsilon -> pure []
               Term c -> pure [c]
--               Anything -> generate $ alt $ alphabeth g
               Kleene x -> generate $ opt (some x)
               Alter a b -> generate a <|> generate b
               Chain a b -> (++) <$> generate a <*> generate b

language :: Grammar a -> [[a]]
language = samples . generate 

           
------------------------------------------------------------
                       
brs f = ch '(' <> many f <> ch ')'

fact f n = if n == 0 then 1 else f(n-1)*n

fix f = f (fix f)

recur n f = foldl1 (.) $ replicate n f
        
mod3 = many (ch 0 <|> (ch 1 <> many (ch 0 <> many (ch 1) <> ch 0) <> ch 1))

arythmetics' num k = term <> many (alt "+-" <> term)
  where
    term = mult <> many (alt "*/" <> mult)
    mult = num <|>
           ch '-' <> mult <|>
           ch '(' <> k <> ch ')'

arythmetics = fix . arythmetics'
              
polynom x = expr
  where
    expr = term <> many (alt "+-" <> term)
    term = opt (ch '-') <>
           ( var <|>
             alt ['2'..'9'] <> ch '*' <> var <|>
             alt ['1'..'9'] )
    var = x <> opt (ch '^' <> alt ['2'..'9'])


------------------------------------------------------------

vanishing :: Grammar a -> Bool
vanishing g = case g of
   Epsilon -> True
   None -> False
   Term _ -> False
--   Anything -> False
   Kleene a -> True
   Alter a b -> vanishing a || vanishing b
   Chain a b -> vanishing a && vanishing b
                        

leader :: Eq a => Grammar a -> [a]
leader g = case g of
   Epsilon   -> mempty
   None      -> empty
   Term c    -> pure c
--   Anything  -> alphabeth g
   Alter a b -> leader a `union` leader b
   Kleene g  -> leader $ opt g
   Chain a b -> leader $ a <|> unless (vanishing a) b

tst = [Epsilon, None, ch 'a', many (ch 'a'), ch 'a' <> ch 'b', ch 'a' <|> ch 'b']
tstf = fmap (,) <$> tst
rule1 u = (pure id <*> u, u)
rule2 u = (u <*> pure 'x', pure ($ 'x') <*> u)
rule3 u v w = (pure (.) <*> u <*> v <*> w , u <*> (v <*> w))

palindrome xs p = oneof (\x -> ch x <|> (ch x <> opt p <> ch x)) xs

simplify :: Eq a => Grammar a -> Grammar a
simplify = fixedPoint go
  where go g = case g of
          Kleene Epsilon -> Epsilon
          Kleene None -> None
          Kleene (Alter Epsilon a) -> Kleene a
          Kleene (Kleene a) -> Kleene a
          Kleene a -> Kleene $ go a
          Alter None a -> a
          Alter a None -> a
          Alter a b | a == b -> a
          Alter a b -> Alter (go a) (go b)
          Chain None _ -> None
          Chain _ None -> None
          Chain Epsilon a -> a
          Chain a Epsilon -> a
          Chain a b -> Chain (go a) (go b)
          x -> x
          
fixedPoint f x = let fx = f x
                 in if fx == x
                    then x
                    else fixedPoint f fx

match :: Grammar Char -> String -> Result String
match = run . go
    where go g = case g of
                   Epsilon -> mempty
                   None -> empty
                   Term '_' -> pure <$> next
                   Term x -> pure <$> check (== x)
                   Kleene a -> mmany (go a)
                   Alter a b -> go a <|> go b
                   Chain a b -> go a <> go b

rpn x f = operand <> ch ' ' <> operand <> ch ' ' <> binary <|>
          operand <> ch ' ' <> unary
    where operand = x <|> f
          binary = alt "+-*/"
          unary = alt "n!"
      
evalRPN = head . runA calcRPN . words
    where calcRPN = Automat Lab4.Any f [] [] []
          f (x:y:s) "+" = (x+y):s
          f (x:y:s) "-" = (y-x):s
          f (x:y:s) "*" = (x*y):s
          f (x:y:s) "/" = (y / x):s
          f (x:s) "n" = (-x):s
          f (x:s) "!" = (product [1..x]):s
          f s n = read n : s

hist lst = foldl' add [] lst

add dict x = case lookup x dict of
               Nothing -> (x,1) : dict
               Just n -> update x (n+1) dict

update x a [] = [(x,a)]
update x a ((y,b):ds)
    | x == y = (x,a):ds
    | x /= y = (y,b) : update x a ds
