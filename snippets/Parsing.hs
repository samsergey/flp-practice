module Parsing where

import Prelude hiding ((>>),(>>=),return, pure)
import Text.Printf
import Lab5 (when)

data Parser i a = Parser { run :: i -> Result i a}

data Result i a = Ok a i | Fail i
  deriving (Show, Eq)

epsilon = Parser $ Ok ()

p1 >> p2 = Parser $ \r -> case run p1 r of
  Ok _ r' -> run p2 r'
  Fail r' -> Fail r'

p1 <|> p2 = Parser $ \r -> case run p1 r of
  Fail _ -> run p2 r
  Ok x r' -> Ok x r'

next = Parser $ \r -> case r of
  x:xs -> Ok x xs
  [] -> Fail r

check p = Parser $ \r -> case r of
  x:xs | p x -> Ok () (x:xs)
  _          -> Fail r 

term c = check (== c) >> next

digit = check (`elem` "0123456789") >> next

end = Parser $ \r -> case r of
  [] -> Ok () []
  r  -> Fail r

neg p = Parser $ \r -> case run p r of
  Ok a i -> Fail r
  Fail i  -> Ok () r
  
------------------------------------------------------------

runTests name ts = when (fails /= []) $
                   print name *> putStr fails
  where
    fails = mconcat $ zipWith test [1..] ts
    test i (p, inp, outp) = when (res /= outp) msg
      where
        res = run p inp 
        msg = unlines $ [ "Fail in test: " <> show i
                        , "  expected: " <> show outp
                        , "  got:      " <> show res ]
  
tests = do
  runTests "term"
    [ (term 'a',             "abab", Ok 'a' "bab")
    , (term 'a',             "bbab", Fail "bbab")
    , (term 'a' >> term 'b', "abab", Ok 'b' "ab")
    , (term 'b' >> term 'b', "abab", Fail "abab") ]

  runTests "next"
    [ (next,         "abab", Ok 'a' "bab")
    , (next >> next, "abab", Ok 'b' "ab")
    , (next,         "",     Fail "") ]

  runTests "digit"
    [ (digit,          "23x", Ok '2' "3x")
    , (digit,          "abc", Fail "abc")
    , (digit >> digit, "23x", Ok '3' "x") ]

  runTests "neg" 
    [ (neg end, "abc", Ok () "abc")
    , (term 'a' >> end, "a", Ok () "")
    , (neg digit, "abc", Ok () "abc")
    , (neg digit, "2bc", Fail "2bc") ]

_A = term 'a' >> term 'b' >> _A >> term 'a' 
     <|> term 'b'

runTestsFor p name tst = runTests name tst'
  where tst' = map (\(i,o) -> (p,i,o)) tst

test_A = runTestsFor _A "A"
    [ ("abba", Ok 'a' "")
    , ("ababbaa", Ok 'a' "")
    , ("abababbaaa", Ok 'a' "")
    , ("aba", Fail "aba")
    , ("ababa", Fail "ababa")] 

_E = _T ?> term '+' ?> _E <|> _T
_T = term '(' ?> _E ?> term ')' <|> _N
_N = digit ?> (_N <|> epsilon)

(?>) :: Parser i a -> Parser i b -> Parser i ()
p1 ?> p2 = p1 >> epsilon >> p2 >> epsilon

test_E = runTestsFor _E "_E"
  [ ("12",            Ok () "" )
  , ("(1+2)+3",       Ok () "" )
  , ("1+(2+3)",       Ok () "" )
  , ("1+2+3",         Ok () "" )
  , ("((123+4))",     Ok () "" )
  , ("(13+4)+6345",   Ok () "" )
  , ("(13+4)+(6+32)", Ok () "" ) ]
