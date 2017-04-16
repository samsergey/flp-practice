data Regexp a = Error
              | Accepting
              | Elem a
              | Or (Regexp a) (Regexp a)
              | And (Regexp a) (Regexp a)
              | Not (Regexp a)
              | Opt (Regexp a)
              | Follow (Regexp a) (Regexp a)
              | Many (Regexp a)
              deriving Show

pd r x = case r of
  Error         -> Error
  Accepting     -> Error
  Elem c
    | c == x    -> Accepting
    | otherwise -> Error
  Or r1 r2      -> pd r1 x `Or` pd r2 x
  And r1 r2     -> pd r1 x `And` pd r2 x
  Not r         -> Not (pd r x)
  Opt r'        -> pd r' x
  Follow r1 r2
    | isError r1     -> Error
    | isAccepting r1 -> (pd r1 x `Follow` r2) `Or` pd r2 x
    | otherwise      -> (pd r1 x `Follow` r2)
  Many r        -> pd r x `Follow` Many r 

isAccepting r = case r of
  Accepting    -> True
  Opt _        -> True
  Many _       -> True
  Or r1 r2     -> isAccepting r1 || isAccepting r2
  And r1 r2    -> isAccepting r1 && isAccepting r2
  Follow r1 r2 -> isAccepting r1 && isAccepting r2
  Not r        -> not (isAccepting r)
  _            -> False

isError r = case r of
  Error        -> True
  Or r1 r2     -> isError r1 && isError r2
  And r1 r2    -> isError r1 || isError r2
  Follow r1 r2 -> isError r1 || isError r2
  _            -> False 

match :: Eq a => Regexp a -> [a] -> Regexp a
match r = foldl pd r


