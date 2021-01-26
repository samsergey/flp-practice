data Free f a = Pure f a
              | Alt (Parser a) (Parser a)
              | Seq (Parser a) (Parser a)
              | Many (Parser a)
  deriving Show

instance Functor Parser where
  fmap f 
