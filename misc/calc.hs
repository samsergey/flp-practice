import Control.Applicative
import Control.Monad

type Command = Token
type Stack = [Token]

data Token = N Double
           | Op String
           | Block [Token]
           | Var String
           deriving Show

readMay :: Read a => String -> Maybe a
readMay s = case [x | (x,t) <- reads s, ("","") <- lex t] of
                [x] -> Just x
                _ -> Nothing

calculate = foldl interprete [] . words
  where
    interprete s op = case op of
      "+" -> binop (+)
      "*" -> binop (*)
      "-" -> binop (-)
      "/" -> binop (/)
      "sqrt" -> case s of
        x:s | x > 0 -> pure $ sqrt x:s
            | x == 0 -> pure $ 0:s
            | x < 0 -> error "negative argument of sqrt"
        []    -> error "got no arguments"
      n   -> case readMay n of
               Just x  -> pure $ x : s
               Nothing -> error ("unknown command " ++ n)

    binop f s = case s of
      x:y:s -> pure $ f x y : s
      [_]   -> error "got only one argument"
      []    -> error "got no arguments"

interpreteA :: (Applicative m, Alternative m, Monad m)
            => [Double] -> [Char] -> m [Double]
interpreteA s op = case op of
  "+" -> binop (+)
  "*" -> binop (*)
  "-" -> binop (-)
  "/" -> binop (/)
  "sqrt" -> case s of
      x:s | x > 0 -> pure (sqrt x:s) <|> pure ((- sqrt x):s)
          | x == 0 -> pure (0:s)
          | x < 0 -> empty
      []    -> empty
  "pm" -> case s of
            x:y:s -> pure (x+y:s) <|> pure (y-x:s)
            _ -> empty
  "dup" -> case s of
      x:s -> pure $ x:x:s
      []  -> empty
  n   -> case readMay n of
           Just x  -> pure $ x : s
           Nothing -> empty
  where
    binop f = case s of
      x:y:s -> pure $ f y x : s
      [_]   -> empty
      []    -> empty

--foldM f x = foldl (\res el -> res >>= (`f` el)) (pure x)
      
--calculate :: String -> Either String State


calculateA :: (Applicative m, Alternative m, Monad m)
            => String -> m [Double]
calculateA = foldM interpreteA [] . words

calculateD input dict = (foldM interpreteD [] $ words input) dict

interpreteD s op = case op of
  "+" -> binop (+)
  "*" -> binop (*)
  "-" -> binop (-)
  "/" -> binop (/)
  n   -> \d -> case readMay n <|> lookup n d of
           Just x  -> x : s
           Nothing -> error ("unknown command " ++ n)
  where
    binop f = case s of
      x:y:s -> pure $ f x y : s
      [_]   -> error "got only one argument"
      []    -> error "got no arguments"

