module Parsing where

import Control.Monad
import Control.Applicative
import Data.Char
import Data.String
import Data.Monoid
import Data.Foldable
import Lab8

data Parser m a = Parser ( State String (m a) )

run (Parser p) s = runState p s

getInput :: (Alternative m) => Parser m get
getInput = Parser get
setInput s = Parser $ set s

instance Functor m => Functor (Parser m) where
  fmap f (Parser p) = Parser $ fmap (fmap f) p

instance Applicative m => Applicative (Parser m) where
  pure x = Parser $ pure (pure x)
  Parser p1 <*> Parser p2 = Parser $ (<*>) <$> p1 <*> p2 

instance Alternative m => Alternative (Parser m) where
  empty = Parser $ pure (empty)
  Parser p1 <|> Parser p2 = Parser $ (<|>) <$> p1 <*> p2 

-- next :: Alternative m => Parser m Char
-- next = do
--   x <- getInput
--   case x of
--     [] -> empty
--     x:xs -> do setInput xs
--                pure x
