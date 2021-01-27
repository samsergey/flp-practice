module Parsing where

import Control.Monad
import Control.Applicative
import Data.Char
import Data.String
import Data.Monoid
import Data.Foldable
import Lab8

data Parser m a = Parser { getRunner :: State String (m a) }

run p s = runState (getRunner p) s

runList :: Parser [] a -> String -> (String, [a])
runList = run

runMaybe :: Parser Maybe a -> String -> (String, Maybe a)
runMaybe = run

instance Functor m => Functor (Parser m) where
  fmap f (Parser p) = Parser $ fmap (fmap f) p

instance Applicative m => Applicative (Parser m) where
  pure x = Parser $ pure (pure x)
  p1 <*> p2 = Parser $ \s -> case run p1 s of
    (s', f) -> case run p2 s' of
      (

instance Alternative m => Alternative (Parser m) where
  empty = Parser $ pure empty
  p1 <|> p2 = Parser . State $ \s -> run p1 s <|> run p2 s
    where run (Parser p) = runState p


check :: Alternative m => (Char -> Bool) -> Parser m Char
check p = Parser $ do
  x <- get
  case x of
    x:xs | p x -> do set xs
                     return (pure x)
    _ -> pure empty

char c = check (== c)

next :: Alternative m => Parser m Char
next = check (const True)
