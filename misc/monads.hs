{-# language DeriveFunctor #-}

import Control.Monad

newtype Dist a = Dist [(a, Int)]
  deriving (Show, Functor)

getDist (Dist d) = d

prop' a = maybe 0 id . lookup a . getDist . clean
prop a d = fromIntegral (prop' a d) / fromIntegral (sum (snd <$> getDist d))

instance Eq a => Semigroup (Dist a) where
 Dist [] <> d2 = d2
 Dist d1 <> Dist d2 = Dist $ foldl addDist d1 d2
   where
     addDist [] (x,n) = [(x,n)]
     addDist ((y,m):xs) (x,n) | x == y = (x,n+m):xs
                              | otherwise = (y,m) : addDist xs (x,n)

instance Eq a => Monoid (Dist a) where
 mempty = Dist []

fromList :: (Foldable t, Eq a) => t a -> Dist a
fromList = clean . foldMap pure

clean (Dist d) = foldMap (\(x,n) -> if n == 0 then mempty else Dist [(x,n)]) d

uniform xs = Dist $ (\x -> (x,1)) <$> xs
dice n = uniform [1..n]
coin = uniform [0,1]
mult n (Dist d) = Dist $ fmap (* n) <$> d

instance Applicative Dist where
  pure x = Dist [(x, 1)]
  (<*>) = ap

instance Monad Dist where
  d >>= f = join $ f <$> d
    where join (Dist ds) = Dist $ ds >>= \(Dist d, n) -> fmap (* n) <$> d


          
          
