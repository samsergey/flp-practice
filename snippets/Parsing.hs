{-# language OverloadedStrings, FlexibleInstances #-}
module Parsing where

import Control.Monad
import Control.Applicative
import Data.Char
import Data.String
import Lab7

data Parser a = Parser { run :: String -> Result a }

data Result a = Ok a String
              | Fail String
              | Error String
  deriving (Show, Eq)

instance Functor Result where
  fmap f r = case r of
    Ok a s -> Ok (f a) s
    Fail s -> Fail s
    Error m -> Error m

instance Functor Parser where
  fmap f p = Parser $ fmap f <$> run p

next = Parser $ \r -> case r of
  x:xs -> Ok x xs
  [] -> Fail r

check p = Parser $ \r -> case r of
  x:xs | p x -> Ok x xs
  _          -> Fail r 

char c = check (== c)
digit = check isDigit
set s = check (`elem` s)

end = Parser $ \r -> case r of
  [] -> Ok () []
  r  -> Fail r

instance Applicative Parser where
  pure x = Parser $ Ok x
  p1 <*> p2 = Parser $ \s ->
    case run p1 s of
      Ok f s' -> f <$> run p2 s'
      Fail s' -> Fail s'
      Error m -> Error m

string s = sequenceA $ char <$> s

instance IsString (Parser String) where
  fromString = string

rep n p = sequenceA $ replicate n p

instance Alternative Parser where
  empty = Parser Fail
  p1 <|> p2 = Parser $ \s ->
    case run p1 s of
      Ok x s' -> Ok x s'
      Fail _ -> run p2 s
      Error m -> Error m

-- instance Monad Parser where
--   p >>= f = Parser $ \s -> case run p s of
--                              Ok x s' -> run (f x) s'
--                              Fail s' -> Fail s'

epsilon :: Parser ()
epsilon = pure ()
 
neg p = Parser $ \r -> case run p r of
  Ok a i -> Fail r
  Fail i  -> Ok () r

_E = (_T *> char '+' *> _E) <|> _T
_T = (char '(' *> _E <* char ')') <|> _N
_N = digit *> (_N <|> epsilon)

-- ------------------------------------------------------------

-- runTests name ts = if fails /= []
--                    then print name >> putStr fails
--                    else mempty
--   where
--     fails = mconcat $ zipWith test [1..] ts
--     test i (p, inp, outp) = if res /= outp then msg else mempty
--       where
--         res = run p inp 
--         msg = unlines $ [ "Fail in test: " <> show i
--                         , "  expected: " <> show outp
--                         , "  got:      " <> show res ]
  
-- tests = do
--   runTests "term"
--     [ (term 'a',             "abab", Ok 'a' "bab")
--     , (term 'a',             "bbab", Fail "bbab")
--     , (term 'a' >> term 'b', "abab", Ok 'b' "ab")
--     , (term 'b' >> term 'b', "abab", Fail "abab") ]

--   runTests "next"
--     [ (next,         "abab", Ok 'a' "bab")
--     , (next >> next, "abab", Ok 'b' "ab")
--     , (next,         "",     Fail "") ]

--   runTests "digit"
--     [ (digit,          "23x", Ok '2' "3x")
--     , (digit,          "abc", Fail "abc")
--     , (digit >> digit, "23x", Ok '3' "x") ]

--   runTests "neg" 
--     [ (neg end,         "abc", Ok () "abc")
--     , (term 'a' >> end, "a",   Ok () "")
--     , (neg digit,       "abc", Ok () "abc")
--     , (neg digit,       "2bc", Fail "2bc") ]

-- _A = (term 'a' >> term 'b' >> _A >> term 'a')
--      <|> term 'b'

-- runTestsFor p name tst = runTests name tst'
--   where tst' = map (\(i,o) -> (p,i,o)) tst

-- test_A = runTestsFor _A "A"
--     [ ("abba", Ok 'a' "")
--     , ("ababbaa", Ok 'a' "")
--     , ("abababbaaa", Ok 'a' "")
--     , ("aba", Fail "aba")
--     , ("ababa", Fail "ababa")] 

-- _E = _T ?> term '+' ?> _E <|> _T
-- _T = term '(' ?> _E ?> term ')' <|> _N
-- _N = digit ?> (_N <|> epsilon) 

-- (?>) :: Parser a -> Parser b -> Parser ()
-- p1 ?> p2 = p1 >> epsilon >> p2 >> epsilon

-- test_E = runTestsFor _E "_E"
--   [ ("12",            Ok () "" )
--   , ("(1+2)+3",       Ok () "" )
--   , ("1+(2+3)",       Ok () "" )
--   , ("1+2+3",         Ok () "" )
--   , ("((123+4))",     Ok () "" )
--   , ("(13+4)+6345",   Ok () "" )
--   , ("(13+4)+(6+32)", Ok () "" ) ]

only p = (:[]) <$> p

instance Semigroup a => Semigroup (Parser a) where
  p1 <> p2 = (<>) <$> p1 <*> p2

instance Monoid a => Monoid (Parser a) where
  mempty = pure mempty
