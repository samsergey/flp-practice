import Control.Monad.Logic

from :: [a] -> Logic a
from = msum . fmap pure

prob1 = do
  i <- from ["the", "that", "a"]
  j <- from ["frog", "elephant", "thing", "turtle"]
  joins i j
  k <- from ["walked", "eats", "treaded", "grows"]
  joins j k
  l <- from ["slowly", "quickly", "salad"]
  joins k l
  return $ unwords [i,j,k,l]
  where joins w1 w2 = guard $ head w2 == last w1

prob2 = do
  x <- from [4..100]
  c <- from [1..100 `div` x]
  p <- from [1..25]
  let s = 100 - c - p
  guard $ c*2*x + p*8 + s == 200
  return [x,c,p,s]


  
  
