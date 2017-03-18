data Env = Env { a :: Int, b :: String } deriving Show


seta x (Env _ b) = Env x b 
setb s (Env a _) = Env a s
