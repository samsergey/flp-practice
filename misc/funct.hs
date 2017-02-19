{-# LANGUAGE DeriveFunctor
           , LambdaCase
           , TupleSections
           , FlexibleInstances
#-}
module CSV where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Safe
--import Data.List (unfoldr,uncons)

-- (<$$>) :: (Functor f1, Functor f) => (a -> b) -> f (f1 a) -> f (f1 b)
-- (<$$>) = fmap . fmap

-- readCSV :: String -> [[String]]
-- readCSV = splitBy ',' <$$> splitBy '\n'

-- splitBy :: Eq a => a -> [a] -> [[a]]
-- splitBy sep = unfoldr (break (== sep) <$$> tailMay) . (sep :)

-- data Person = Person Int String Int deriving Show

-- readPerson = parse (Person <$> int <*> pop <*> int)
--   where int = try readNum
        
-- parse p = fst <$$> run p

-- ------------------------------------------------------------

-- calc = foldr operate [] . reverse . words

-- operate "+" (x:y:s) = x+y : s
-- operate "*" (x:y:s) = x*y : s
-- operate n s = read n : s

--foldM' f x = foldl (\res el -> res >>= (`f` el)) (pure x)

--------------------------------------------------------------------------------

class (Alternative m, Monad m) => Failable m where
  err :: String -> m a
  err _ = empty

instance Failable Maybe 
instance Failable []    

errIf p m x = if p x then err m else pure x

--------------------------------------------------------------------------------

newtype Parser f s r = Parser {run :: s -> f (r, s)} deriving Functor

instance Failable f => Applicative (Parser f s) where
   pure x = Parser (\s -> pure (x, s))
   (<*>) = ap

instance Failable f => Monad (Parser f s) where
   x >>= f = join (f <$> x)
      where join p = Parser (run p >=> uncurry run)
  
instance Failable f => Alternative (Parser f s) where
  empty = Parser (\_ -> empty)
  p1 <|> p2 = Parser (\s -> run p1 s <|> run p2 s)

instance Failable f => Failable (Parser f s) where
  err m = Parser (\_ -> err m)

pop :: Failable f => Parser f [a] a
pop = Parser (\case [] -> empty
                    x:s -> pure (x, s))

push x = Parser (\s -> pure (x, x:s))

--------------------------------------------------------------------------------

type Command = String
type Stack = [Double]

calculate :: Failable f => String -> f Stack
calculate = foldM interprete [] . words

interprete :: Failable f => Stack -> Command -> f Stack
interprete s op = case op of
  "+"    -> (+) <$> pop1 <*> pop2  >>>  push
  "*"    -> (*) <$> pop1 <*> pop2  >>>  push
  "-"    -> flip (-) <$> pop1 <*> pop2  >>>  push
  "/"    -> flip (/) <$> nonzero <*> pop2  >>>  push
  "+-"   -> plusMinus <$> pop1 <*> pop2  >>>  pushSplit 
  "sqrt" -> plusMinus <$> sqrt <$> nonnegative <*> pure 0  >>>  pushSplit
  "dup"  -> pop1  >>>  \(x,s) -> pure (x:x:s)
  "swap" -> (,) <$> pop1 <*> pop2  >>>  \((x,y),s) -> pure (y:x:s)
  n      -> tryRead n  >>>  push

  where

    infix 1 >>>
    p >>> f = (run p s >>= f)
      <|> err ("while processing '" ++ op ++ "'. Stack: " ++ show (take 4 s))
 
    pop1 = pop <|> err "got no arguments "

    pop2 = pop <|> err "got only one argument "

    nonzero = pop1 >>= errIf (== 0) "expected nonzero number "

    nonnegative = pop1 >>= errIf (< 0) "expected nonnegative number "

    push (x,s) = pure (x:s)

    pushSplit ([],s) = empty
    pushSplit (x:xs,s) = pure (x:s) <|> pushSplit (xs,s)

    plusMinus 0 x = [x]
    plusMinus y x = [x+y,x-y]

    tryRead n = case readMay n of
      Just x -> pure x
      Nothing -> err "unknown command "

--------------------------------------------------------------------------------

instance Monoid m => Alternative (Either m) where
  empty = Left mempty
  Left m1 <|> Left m2 = Left (m1 <> m2)
  Left _ <|> x = x
  x <|> _ = x

instance Failable (Either String) where err = Left

instance Failable IO where err = error

--------------------------------------------------------------------------------

main s = do print (calculate s :: Maybe Stack)
            print (calculate s :: [Stack])
            print (calculate s :: Either String Stack)
            calculate s :: IO Stack


-- import Safe
-- import Control.Monad ((>=>))
-- import Control.Applicative
-- import Data.Functor.Compose

-- data Person = Person String Int Bool deriving (Show)

-- lookPerson' s = Person
--   <$> (lookup "name" s <|> lookup "Name" s)
--   <*> (lookup "age" s >>= readMay)
--   <*> (lookup "married" s >>= readMay)

-- lookPerson = getCompose $
--   Person
--   <$> Compose (lookup "name" <||> lookup "Name")
--   <*> Compose (lookup "age" >=> readMay)
--   <*> Compose (lookup "married" >=> readMay)

-- (<||>) = liftA2 (<|>)
