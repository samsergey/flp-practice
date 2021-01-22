import Lab8 (State, runState, get, set)

data Parser m i a = Parser { runParser :: i -> Either String (State i (m a)) }

instance Alternative m => Monad (Parser m i) where
  t >>= f = Parser $ \i -> case runParser t i of
    Right r -> Right $ runState $ do i' <- get
                                     r' <- 
                                     
    Left m -> Left m
