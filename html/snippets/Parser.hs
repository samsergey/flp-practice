import Data.Monoid
import Data.Ord
import Data.Char
import Data.Foldable hiding (elem)
import Control.Monad
import Control.Applicative
import Pic

data Parser i a = Parser { run :: i -> Result i a}


data Result i a = Ok a i
                | Fail i
                | Error String
  deriving (Show, Eq)

instance Monad (Parser i) where
  return = pure
  t >>= f = Parser $ \r -> case run t r of
    Ok x r' -> run (f x) r'
    Fail r' -> Fail r'
    Error m -> Error m

instance Functor (Parser i) where
  fmap f p = p >>= (pure . f)

instance Applicative (Parser i) where
  pure = Parser . Ok
  f <*> x = f >>= (<$> x)

instance Alternative (Parser i) where
  empty = Parser Fail
  t1 <|> t2 = Parser $ \r -> case run t1 r of
    Fail _ -> run t2 r
    Ok x r' -> Ok x r'
    Error m -> Error m

instance Semigroup b => Semigroup (Parser i b) where
  a <> b = (<>) <$> a <*> b

instance Monoid b => Monoid (Parser i b) where
  mempty = pure mempty

err m = Parser $ const (Error m)
epsilon = Parser $ Ok ()

notEnd = Parser $ \r -> case r of
  [] -> Fail []
  _ -> Ok () r

next = notEnd >> (Parser $ \(x:xs) -> Ok x xs)
    
check p = notEnd >> (Parser $ \r -> if p (head r)
                                    then Ok () r
                                    else Fail r)

neg p = Parser $ \r -> case run p r of
  Fail _ -> Ok () r
  Ok _ _ -> Fail r

term c = check (== c) >> next

string = mapM term 

integer :: Parser String Integer
integer = read <$> some digit

digit = check (`elem` ['0'..'9']) >> next

p `oneof` lst = asum $ p <$> lst

msome p = mconcat <$> some p
mmany p = mconcat <$> many p
mopt p = mconcat <$> opt p

opt p = toList <$> optional p

only p = (:[]) <$> p

--collect p = (p <|> mempty <$ next) <> (collect p <|> mempty)
collect p = mmany (p <|> mempty <$ next) 

count p = getSum <$> collect (Sum 1 <$ p)

longest p = maximumBy (comparing length) <$> search p

search p = collect (only p)

p <?> pred = p >>= \r -> if pred r then pure r else empty

skip p = p >> epsilon 

p1 ?> p2 = p1 >> epsilon >> p2 >> epsilon

_E = _T ?> term '+' ?> _E <|> _T
_T = term '(' ?> _E ?> term ')' <|> _N
_N = digit ?> (_N <|> epsilon)

bracket = term '(' ?> mmany bracket ?> term ')'

end = neg notEnd

------------------------------------------------------------

-- nonzero = (next <?> (/=0)) <|> err "expected non zero value!"

-- push x = Parser $ \r -> Ok () (x:r)

-- calcRPN expr = run (foldMap interprete $ words expr) []

-- interprete op = op <:> case op of
--   "+" -> binop (+) next next
--   "-" -> binop (-) next next
--   "/" -> binop div nonzero next
--   n -> case readMay n of
--     Nothing -> err "is not a number!"
--     Just x -> push x

-- binop f p1 p2 = "expected two arguments, got" <:> do
--   x <- p1 <|> err "none!"
--   y <- p2 <|> err "one!"
--   push (f y x)

-- s <:> p = Parser $ \r -> case run p r of
--   Error m -> Error (s ++ ' ' : m)
--   x -> x
------------------------------------------------------------

data Person = Person { name :: String
                     , age :: Int
                     , hobby :: [String] } deriving Show

person = Person <$> string_
                <*> number_
                <*> listOf string_

readPerson = parse

number_ = read <$> some digit

string_ = term '\'' *> some (term `except` "'") <* term '\''

bool_ = (True <$ string "true") <|> (False <$ string "false")

listOf p = ((p `sepBy` term ',') <|> pure [])

clean = collect $
  (string "'" <> some (term `except` "'") <> string "'") <|>
  only (term `except` " \n\t")

parse p s = case run p s of
  Ok r _ -> Just r
  Fail _ -> Nothing

readCSV p = parse clean >=> parse p 
------------------------------------------------------------

-- ChainL = p {o p}
-- p1 o p2 o p3 = (o p3) . (o p2 ) $ p1
chainl1 p o = p <**> (appEndo . getDual <$> mmany terms)
  where
    terms = Dual . Endo <$> ((flip <$> o) <*> p)

-- ChainR = {p o} p
-- p1 o p2 o p3 = (p1 o). (p2 o) $ p3
chainr1 p o = appEndo <$> mmany terms <*> p
  where
    terms = Endo <$> (p <**> o)

chainl x0 p o = (appEndo . getDual) <$> mmany terms <*> pure x0
  where
    terms = Dual . Endo <$> (p <**> (flip <$> o <|> pure const))

chainr x0 p o = appEndo <$> mmany terms <*> pure x0
  where
    terms =  Endo <$> (p <**> (o <|> pure const))

pfoldMap m p = mmany (m <$> p)

add = (+) <$ term '+'
sub = (-) <$ term '-'

foldr'' f x0 t = appEndo (foldMap (Endo . f) t) x0

foldl'' f x0 t = getDual (foldMap (Dual . Endo . flip f) t) `appEndo` x0


sepBy p s = only p <> many (s *> p)

except p xs = neg (p `oneof` xs) *> next

data JSON = N Int
          | S String
          | B Bool
          | A [JSON]
          | O [(String, JSON)]
          deriving Show

json = N <$> number_ <|>
       S <$> string_ <|>
       B <$> bool_ <|>
       A <$> listOf json `between` "[]" <|>
       O <$> listOf (pairOf string_ json) `between` "{}"
  where
    pairOf p1 p2 = (,) <$> p1 <*> (term ':' *> p2)

between p [s1,s2] = term s1 *> p <* term s2



-- readJSON = parse cleanJSON >=> parse json

------------------------------------------------------------

regexp_ = chainr1 (msome element) alt
  where
    alt = (<|>) <$ term '|'
    element = (group <|> (only <$> symbol)) <**> modifier
    group = regexp_ `between` "()"
    symbol = anychar <|> charClass <|> literal 

literal = term <$> term `except` "?+*()[]|."
anychar = next <$ term '.'

charClass = c `between` "[]"
   where
     c = except term <$> (term '^' *> chars)
         <|> oneof term <$> chars
     chars = msome (range <|> only char)
     char = term `except` "]"
     range = enumFromTo <$> char <*> (term '-' *> char)

modifier = option <|> repeat0 <|> repeat1 <|> pure id
  where
    option = mopt <$ term '?'
    repeat1 = msome <$ term '+'
    repeat0 = mmany <$ term '*'

regexp s = case run regexp_ s of
  Ok p "" -> p
  _ -> empty

params = search (regexp "[a-z]+=[^&]+")

replace :: Parser [b] a -> (a -> [b]) -> Parser [b] [b]
replace p f = collect (f <$> p <|> only next)

------------------------------------------------------------
--svg = tag "svg" $ many (point_ <|> line_)


float :: Parser String Float
float = read <$> regexp "-?[0-9]+[.][0-9]+"

spaces = many (term ' ')

attr' a p = spaces >> string a >> string "='" *> p <* string "'" 

tag t p = term '<' >> string t *> p <* string "/>"

tag' t attrs = term '<' >> string t *> sequenceA attrs <* string "/>"

point_ = tag' "circle" [ attr' "rx" float
                       , attr' "ry" float
                       , attr' "r" float ] >>=
         \[x,y,_] -> return $ Point (Pt x y)

line_ = tag' "polyline" [attr' "points" points_] >>=
        \[x] ->  return $ Line x
  
points_ = pt `sepBy` spaces <* spaces
  where pt = Pt <$> float <* term ',' <*> float
