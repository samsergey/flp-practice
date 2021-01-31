module Parsing where

import Control.Monad
import Control.Applicative
import Data.Char
import Data.String
import Data.Monoid
import Data.Foldable
import Lab7
import Lab6

data Parser a = Parser { run :: String -> Result a }

data Result a = Ok a String
              | Fail String
              | Error String
  deriving (Show, Eq)

instance Functor Result where
  fmap f r = case r of
    Ok a s -> Ok (f a) s
    Fail s -> Fail s
    Error s -> Error s

instance Semigroup a => Semigroup (Parser a) where
  p1 <> p2 = (<>) <$> p1 <*> p2

instance Monoid a => Monoid (Parser a) where
  mempty = pure mempty
  
instance Functor Parser where
  fmap f p = Parser $ fmap f <$> run p

instance Applicative Parser where
  pure x = Parser $ Ok x
  p1 <*> p2 = Parser $ \s ->
    case run p1 s of
      Ok f s' -> f <$> run p2 s'
      Fail s' -> Fail s'
      Error s -> Error s

instance Alternative Parser where
  empty = Parser Fail
  p1 <|> p2 = Parser $ \s ->
    case run p1 s of
      Ok x s' -> Ok x s'
      Fail _ -> run p2 s
      Error s -> Error s

instance Monad Parser where
  p >>= f = Parser $ \s ->
    case run p s of
      Ok x s' -> run (f x) s'
      Fail s' -> Fail s'
      Error s -> Error s

------------------------------------------------------------

next = Parser $ \r -> case r of
  x:xs -> Ok x xs
  [] -> Fail r

check p = Parser $ \r -> case r of
  x:xs | p x -> Ok x xs
  _          -> Fail r 

char c = check (== c)
digit = check isDigit
set s = check (`elem` s)

except p xs = neg (p `oneof` xs) *> next

end = Parser $ \r -> case r of
  [] -> Ok () []
  r  -> Fail r

string :: String -> Parser String
string s = sequenceA $ char <$> s

rep n p = sequenceA $ replicate n p

epsilon :: Parser ()
epsilon = pure ()

err :: String -> Parser a
err m = Parser $ \_ -> Error m

getInput :: Parser String
getInput = Parser $ \s -> Ok s s


p !> m = p <|> (getInput >>= err . msg)
  where
    msg s = let r = if null s then "end of line" else s
            in "Expected " <> m <> ", got " <> r

p @> m = Parser $ \s -> case run p s of
                          Error e -> Error (e <> " in " <> m)
                          Fail x -> Fail x
                          Ok a x -> Ok a x
  
neg p = Parser $ \r -> case run p r of
  Ok a i -> Fail r
  Fail i  -> Ok () r


only p = (:[]) <$> p

p `oneof` lst = asum $ p <$> lst

msome p = mconcat <$> some p
mmany p = mconcat <$> many p
mopt p = mconcat <$> opt p

opt p = toList <$> optional p



skip p  = const () <$> p

data XML = Tag String [XML]
         | Text String
  deriving Show

try p = p <|> empty

xml = tag <|> text
  where
    text = Text <$> some (check (/= '<'))
    tag = try $ do
      char '<'
      t <- some $ check (/= '>')
      char '>'
      c <- many xml
      string "</"
      string t <|> err ("Unclosed tag " <> t)
      char '>'
      return $ Tag t c


 
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
--   runTests "char"
--     [ (char 'a',             "abab", Ok 'a' "bab")
--     , (char 'a',             "bbab", Fail "bbab")
--     , (char 'a' >> char 'b', "abab", Ok 'b' "ab")
--     , (char 'b' >> char 'b', "abab", Fail "abab") ]

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
--     , (char 'a' >> end, "a",   Ok () "")
--     , (neg digit,       "abc", Ok () "abc")
--     , (neg digit,       "2bc", Fail "2bc") ]

-- _A = (char 'a' >> char 'b' >> _A >> char 'a')
--      <|> char 'b'

-- runTestsFor p name tst = runTests name tst'
--   where tst' = map (\(i,o) -> (p,i,o)) tst

-- test_A = runTestsFor _A "A"
--     [ ("abba", Ok 'a' "")
--     , ("ababbaa", Ok 'a' "")
--     , ("abababbaaa", Ok 'a' "")
--     , ("aba", Fail "aba")
--     , ("ababa", Fail "ababa")] 

-- _E = _T ?> char '+' ?> _E <|> _T
-- _T = char '(' ?> _E ?> char ')' <|> _N
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

chainr :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr p o = appEndo <$> mmany chars <*> p
  where
    chars = Endo <$> (p <**> o)

chainl :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl p o = p <**> (appEndo . getDual <$> mmany chars)
  where
    chars = Dual . Endo <$> ((flip <$> o) <*> p)

add = (+) <$ char '+'
sub = (-) <$ char '-'
mul = (*) <$ char '*'
frac = div <$ char '/'

integer :: Parser Int
integer = read <$> some digit

unexpected = getInput >>= err . msg
  where msg s = "Unexpected symbol " <> s

expr = _E
  where _E = _T `chainl` (add <|> sub)
        _T = _P `chainl` (mul <|> frac)
        _P = char '(' *> _E <* (char ')' !> "closing parenthesis")
             <|> (integer !> "number")

        add = (+) <$ char '+'
        sub = (-) <$ char '-'
        mul = (*) <$ char '*'
        frac = div <$ char '/'

between p [c1,c2] = char c1 *> p <* char c2

collect p = mmany (p <|> mempty <$ next) 
search p = collect (only p)
------------------------------------------------------------

string_ = char '\'' *> some (char `except` "'") <* char '\''

sepBy p s = only p <> many (s *> p)

chainr1 p o = appEndo <$> mmany terms <*> p
  where
    terms = Endo <$> (p <**> o)


regexp_ = chainr1 (msome element) alt
  where
    alt = (<|>) <$ char '|'
    element = (group <|> (only <$> symbol)) <**> modifier
    group = regexp_ `between` "()"
    symbol = anychar <|> charClass <|> literal 

literal = char <$> char `except` "?+*()[]|."
anychar = next <$ char '.'

charClass = c `between` "[]"
   where
     c = except char <$> (char '^' *> chars)
         <|> oneof char <$> chars
     chars = msome (range <|> only lit)
     lit = char `except` "]"
     range = enumFromTo <$> lit <*> (char '-' *> lit)

modifier = option <|> repeat0 <|> repeat1 <|> pure id
  where
    option = mopt <$ char '?'
    repeat1 = msome <$ char '+'
    repeat0 = mmany <$ char '*'

regexp s = case run regexp_ s of
  Ok p "" -> p
  _ -> empty

params = search (regexp "[a-z]+=[^&]+")

replace p f = collect (f <$> p <|> only next)

------------------------------------------------------------

float :: Parser Float
float = read <$> regexp "-?[0-9]+([.][0-9]*)?"

spaces = many (char ' ')

identifier = regexp "[a-zA-Z][a-zA-Z0-9_-]*"

type Attr = (String, String)

tag :: String -> Parser b -> Parser ([Attr], b)
tag t p = do
  char '<' *> string t
  attrs <- many attr
  spaces *> char '>'
  contents <- p
  string "</" *> string t *> char '>'
  return (attrs, contents)

tag' :: String -> Parser [Attr]
tag' t = do
  char '<' *> string t
  attrs <- many attr
  spaces *> string "/>"
  return attrs

attr :: Parser Attr
attr = do
  a <- spaces *> identifier
  v <- spaces *> char '=' *> spaces *> string_
  return (a,v)

getAttr :: Parser v -> String -> [Attr] -> Parser v
getAttr p a as = Parser $ \s ->
  case lookup a as of
    Nothing -> Fail s
    Just x -> case run p x of
                Ok r _ -> Ok r s
                Fail _ -> Fail s

point_ = do
  as <- tag' "circle"
  r <- getAttr float "r" as
  guard $ r == 1
  x <- getAttr float "rx" as
  y <- getAttr float "ry" as
  return $ Point (Pt x y) 

line_ = do
  as <- tag' "polyline"
  pts <- getAttr points_ "points" as
  return $ Line pts

points_ = pt `sepBy` spaces <* spaces
  where pt = Pt <$> float <* char ',' <*> float

group_ = do
  (as, ps) <- tag "g" primitives
  attrs <- findAttr LineColor identifier "stroke" <>
           findAttr Fill identifier "fill" <>
           findAttr LineWidth float "stroke-width" <>
           findAttr Opacity float "fill-opacity" $ as
  return $ Group attrs ps
  where
    findAttr a p s as = mopt . only $ a <$> getAttr p s as

primitives = many $ point_ <|> line_ <|> group_
  
picture = foldMap primitive . snd <$> tag "svg" primitives


------------------------------------------------------------

