type Space s m = [(m , s)]
type Strategy s m = Space s m -> Space s m -> Space s m

space :: Monoid m =>
         Strategy s m -> (s -> Space s m) -> (s -> Space s m)
space f trans s = expand f (step (mempty, s))
    where expand f [] = mempty
          expand f (s:ss) = s:expand f (f (step s) ss)
          step (ms, s) = [(ms <> m, t) | (m, t) <- trans s]

search f trans isSolution = filter isSolution . space f trans

--search_dfs = search (<>)
--search_bfs = search $ flip (<>)


------------------------------------------------------------

prefix p s = go p s
  where
    go [] str = [([p], str)]
    go (x:xs) (y:ys) | x == y = go xs ys
    go _ _ = mempty

wordSplit dict s = unwords.fst <$> search (<>) trans isSolution s
  where
    trans = foldMap prefix dict
    isSolution (_, s) = null s 

dict = words "i like and ice cream icecream mango man go"

------------------------------------------------------------

