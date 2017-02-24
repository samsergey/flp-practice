import Data.Monoid
import Data.Ord
import Data.Foldable
import Control.Monad
import Control.Applicative
import Safe

data Parser i a = Parser { run :: [i] -> Result [i] a}


data Result i a = Ok a i
                | Fail i
                | Error String
  deriving (Show, Eq)

instance Monad (Parser i) where
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

instance Monoid b => Monoid (Parser i b) where
  a `mappend` b = mappend <$> a <*> b
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

term c = check (== c) >> next

string :: Eq a => [a] -> Parser a [a]
string = mapM term 

integer :: Parser Char Integer
integer = read <$> some digit

digit = check (`elem` ['0'..'9']) >> next

--oneof :: (b -> Parser i a) -> [b] -> Parser i a
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

------------------------------------------------------------

nonzero = (next <?> (/=0)) <|> err "expected non zero value!"

push x = Parser $ \r -> Ok () (x:r)

calcRPN expr = run (foldMap interprete $ words expr) []

interprete op = op <:> case op of
  "+" -> binop (+) next next
  "-" -> binop (-) next next
  "/" -> binop div nonzero next
  n -> case readMay n of
    Nothing -> err "is not a number!"
    Just x -> push x

binop f p1 p2 = "expected two arguments, got" <:> do
  x <- p1 <|> err "none!"
  y <- p2 <|> err "one!"
  push (f y x)

s <:> p = Parser $ \r -> case run p r of
  Error m -> Error (s ++ ' ' : m)
  x -> x
------------------------------------------------------------
