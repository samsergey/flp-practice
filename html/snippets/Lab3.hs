module Lab3 where

pascalStep r = zipWith (+) (0:r) (r++[0])

pascals = iterate pascalStep [1]

binomial n k = pascals !! n !! k

bernoulli n k = (tail . scanl (+) 0 <$> pascals) !! n !! k

binomial' n k = product [n - k + 1..n] `div` product [1..k]

floyd = go 1 [1..]
  where go i xs = let (a, b) = splitAt i xs
                  in a : go (i+1) b


