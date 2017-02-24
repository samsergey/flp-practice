import Safe
import Control.Applicative
import Control.Monad

type Command = String
type Stack = [Double]


interprete s op = case op of
  "+" -> binop (+)
  "*" -> binop (*)
  "-" -> binop (-)
  "/" -> binop (/)
  "dup" -> case s of
      x:s -> Right $ x:x:s
      []    -> Left "got no arguments"
  n   -> case readMay n of
           Just x  -> Right $ x : s
           Nothing -> Left ("unknown command " ++ n)
  where
    binop f = case s of
      x:y:s -> Right $ f x y : s
      [_]   -> Left "got only one argument"
      []    -> Left "got no arguments"

--foldM f x = foldl (\res el -> res >>= (`f` el)) (pure x)
      
--calculate :: String -> Either String State
--calculate = foldM interprete [] . words

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
    binop f = \_ -> case s of
      x:y:s -> f x y : s
      [_]   -> error "got only one argument"
      []    -> error "got no arguments"

calculateW input = foldM interpreteW mempty $ words input

interpreteW s op = case op of
  "+" -> binop (+)
  "*" -> binop (*)
  "-" -> binop (-)
  "/" -> binop (/)
  n   -> case readMay n of
           Just x  -> push x s
           Nothing -> err ("unknown command" ++ n)
  where
    binop f = tell ("perform " ++ op) >> case s of
      x:y:s -> tell "pushed result" >> push (f x y) s
      [_]   -> err "got only one argument"
      []    -> err "got no arguments"

    tell m = ([m],())
    push x s = tell ("pushed " ++ show x) >> pure (x:s)
    err m = tell ("error! " ++ m) >> pure s
