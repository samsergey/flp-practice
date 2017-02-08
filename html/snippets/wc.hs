{-# Language Strict #-}
import Data.Monoid
import Data.List
import Control.Arrow

wc :: String -> (Int, Int, Int)
wc s = getResult $ foldMap count $ zip s (tail s) 
  where
    getResult (a, (b, c)) = (getSum a, getSum b, getSum c)

    count = countWords &&& countLines &&& countChars 

    countWords = when (rBoundary " \n") (Sum 1)
    countLines = when ((== '\n') . fst) (Sum 1)
    countChars = const (Sum 1)
    
    rBoundary set (x, y) = not (x `elem` set) && y `elem` set

when :: Monoid t1 => (t -> Bool) -> t1 -> t -> t1
when p m x = if p x then m else mempty

wc' :: String -> (Int, Int, Int)
wc' s = (length $ words s, length $ lines s, length s) 

--length' = getSum . foldMap (const 1)

main :: IO ()
main = do
  -- txt <- readFile "../emacs-ref.html"
  txt <- readFile "txt"
  print $ wc txt


power :: (Monoid a, Integral i) => a -> i -> a
power _ 0 = mempty
power a n
  | r == 0    = b
  | otherwise = a `mappend` b
  where
    (q,r) = n `divMod` 2
    b = (a `mappend` a) `power` q


data M a = M [[a]] | Unit deriving Show

instance Num a => Monoid (M a) where
  mempty = Unit
  Unit `mappend` x = x
  x `mappend` Unit = x
  M m1 `mappend` M m2 = M [ [ r <.> c |  c <- transpose m2] |r <- m1 ]
    where a <.> b = sum $ zipWith (*) a b


rotate a = M [[cos a, - sin a, 0],[sin a, cos a, 0], [0,0,1]]
scale s = M [[s, 0, 0],[0, s, 0], [0,0,1]]
shift x y = M [[1, 0, x],[0, 1, y], [0,0,1]]

transform m v = (<> v) <$> m

model :: [M Double] -> [M Double]
model = foldMap $ transform [ scale 0.5 <> rotate (-pi/2) <> shift 0 1
                            , scale 0.5 <> shift 0 1
                            , scale 0.5 <> rotate (pi/2) <> shift 0 1]
