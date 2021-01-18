module Lab3 where

pascalStep r = zipWith (+) (0:r) (r++[0])

pascals = iterate pascalStep [1]

binomial n k = pascals !! n !! k

bernoulli n k = ((tail . scanl (+) 0) <$> pascals) !! n !! k

binomial' n k = product [n - k + 1..n] `div` product [1..k]

floyd = reverse <$> go 0 [] 1
  where go 0 r i = (i:r) : go (length r + 1) [] (i + 1)
        go j r i = go (j - 1) (i:r) (i + 1)

floyd' = go 1 [1..]
  where go i = let (a, b) = splitAt i xs
