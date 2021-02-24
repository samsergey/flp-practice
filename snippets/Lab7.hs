{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Lab7 where

import Lab1 (mean)
import Lab4 (fromBase)
import Lab5 ((*<>))
import Control.Applicative hiding (many, some)
import Control.Monad (foldM, ap)
import Logic
import Data.List
import Data.Ord
    
unless test p = if test then p else empty
untill test x p = if test then pure x else p

bisection p (a,b) | p a == p b = empty
                  | (b-a) < 1e-11 = pure c
                  | otherwise =
                      bisection p (a,c) <|> bisection p (c,b)
  where c = mean a b
       
        
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

------------------------------------------------------------
                   
data Grammar a =
  Epsilon                         -- пустой символ
  | Fail                          -- невозможный символ
  | Term a                        -- литерал
  | Kleene (Grammar a)            -- звезда Клини (повторение)
  | Alt (Grammar a) (Grammar a)   -- объединение (альтернатива)
  | Chain (Grammar a) (Grammar a) -- цепочка (конкатенация)
    deriving (Show, Functor)

toString Epsilon = ""
toString Fail = "⊥"
toString (Term x) = [x]
toString (Kleene (Term x)) = [x] <> "*"
toString (Kleene x) = "(" <> toString x <> ")*"
toString (Alt x y) = "(" <> toString x <> "|" <> toString y <> ")"
toString (Chain x y) = toString x <> toString y
                         
             
instance Semigroup (Grammar a) where
  (<>) = Chain
    
instance Monoid (Grammar a) where
  mempty = Epsilon

instance Applicative Grammar where
  pure = Term
  (<*>) = undefined

instance Alternative Grammar where
  empty = Fail
  (<|>) = Alt
            
ch = Term
opt x = Epsilon <|> x
str s = foldMap ch s
alt x = asum $ Term <$> x

asum lst = foldr (<|>) empty lst

many x = Kleene x
some x = x <> Kleene x
         
lessThen n x = asum $ take n $ iterate (x <>) mempty

generate r = case r of
   Epsilon -> pure []
   Fail -> empty
   Term c -> pure [c]
   Kleene x -> generate Epsilon <|> generate (x <> Kleene x)
   Alt r1 r2 -> generate r1 <|> generate r2
   Chain r1 r2 -> (++) <$> generate r1 <*> generate r2
                         
------------------------------------------------------------
                       
brs = ch '(' <> many brs <> ch ')'
--      <|> ch '[' <> many brs <> ch ']'
--      <|> ch '{' <> many brs <> ch '}'

          
mod3 = many (ch 0 <|> (ch 1 <> many (ch 0 <> many (ch 1) <> ch 0) <> ch 1))

freeze x = Alt x Fail
       
expr = term <> many (alt "+-" <> term)
term = mult <> many (alt "*/" <> mult)
mult = ch 'x' <|> ch '(' <> expr <> ch ')'
