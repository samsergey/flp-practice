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
  | Term a                        -- литерал
  | Kleene (Grammar a)            -- звезда Клини (повторение)
  | Or (Grammar a) (Grammar a)    -- объединение (альтернатива)
  | Chain (Grammar a) (Grammar a) -- цепочка (конкатенация)
    deriving (Functor, Show, Eq) --,Foldable, Traversable)


             
instance Semigroup (Grammar a) where
  Epsilon <> x = x
  x <> Epsilon = x
  None <> x = None
  x <> None = None
  a <> b = Chain a b
    
instance Monoid (Grammar a) where
  mempty = Epsilon

instance Applicative Grammar where
  pure = Term
  Epsilon   <*> x = Epsilon
  None      <*> x = None
  Term f    <*> x = f <$> x
  Kleene f  <*> x = Kleene (f <*> x)
  Or f g    <*> x = (f <*> x) <|> (g <*> x)
  Chain f g <*> x = (f <*> x) <> (g <*> x)

instance Alternative Grammar where
  empty = None
  None <|> x = x
  x <|> None = x
  a <|> b = Or a b

ch :: a -> Grammar a
ch x = pure x

str, alt :: [a] -> Grammar a
str s = foldMap pure s
alt x = getAlt $ foldMap pure x

opt, many, some :: Grammar a -> Grammar a
opt g = Epsilon <|> g
many g = Kleene g
some g = g <> Kleene g
         
generate :: Alternative f => Grammar a -> f [a]
generate r = case r of
   Epsilon -> pure []
   None -> empty
   Term c -> pure [c]
   Or r1 r2 -> generate r1 <|> generate r2
   Kleene x -> generate $ opt (some x)
   Chain r1 r2 -> (++) <$> generate r1 <*> generate r2

language :: Grammar a -> [[a]]
language = samples . generate

------------------------------------------------------------
                       
brs = ch '(' <> many brs <> ch ')' <|>
      ch '[' <> many brs <> ch ']' <|>
      ch '{' <> many brs <> ch '}'

          
mod3 = many (ch 0 <|> (ch 1 <> many (ch 0 <> many (ch 1) <> ch 0) <> ch 1))

arythmetics :: Grammar Char
arythmetics = expr
  where
    expr = term <> many (alt "+-" <> term)
    term = mult <> many (alt "*/" <> mult)
    mult = num <|> ch '(' <> expr <> ch ')'
    num = alt ['1'..'9']

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
   Kleene a -> True
   Or a b -> vanishing a || vanishing b
   Chain a b -> vanishing a && vanishing b


leader :: Eq a => Grammar a -> [[a]]
leader g = case g of
   Epsilon   -> pure []
   None      -> empty
   Term c    -> pure [c]
   Or a b    -> leader a `union` leader b
   Kleene g  -> leader $ opt g
   Chain a b -> leader $ a <|> unless (vanishing a) b

tst = [Epsilon, None, ch 'a', many (ch 'a'), ch 'a' <> ch 'b', ch 'a' <|> ch 'b']
tstf = fmap (,) <$> tst
rule1 u = (pure id <*> u, u)
rule2 u = (u <*> pure 'x', pure ($ 'x') <*> u)
rule3 u v w = (pure (.) <*> u <*> v <*> w , u <*> (v <*> w))
