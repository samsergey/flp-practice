{-# LANGUAGE DeriveFunctor #-}
{-#language GeneralizedNewtypeDeriving #-}
import Data.Monoid
import Data.List
import Data.Foldable
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


times :: (Monoid a, Integral i) => i -> a -> a
0 `times` _ = mempty
1 `times` a = a
2 `times` a = a <> a
n `times` a | even n = (n` div` 2) `times` (2 `times` a)
            | odd n  = a <> (n` div` 2) `times` (2 `times` a)


data M a = M [[a]] | I deriving Show

instance Num a => Monoid (M a) where
  mempty = I
  I `mappend` x = x
  x `mappend` I = x
  M m1 `mappend` M m2 = M [ [ r `dot` c |  c <- transpose m2 ] | r <- m1 ]
    where a `dot` b = sum $ zipWith (*) a b

--------------------------------------------------------------------------------

fib 0 = 1
fib n = a
  where M ((a:_):_) = n `times` M [[1,1,0],[1,0,0],[0,1,0]]

fibs = 1 : scanl' (+) 1 fibs

--------------------------------------------------------------------------------
data Primitive a = Line [a]
                 | Point a deriving (Show,Functor)

primTransform m p = trans <$> p
  where trans (x,y) = let M [[x'],[y'],_] = m <> M [[x],[y],[1]] in (x',y')

rotate a = M [[c, -s, 0], [s,  c, 0], [0,  0, 1]]
  where c = cos a
        s = sin a
        
scale s = M [[s, 0, 0], [0, s, 0], [0, 0, 1]]

shift x y = M [[1, 0, x], [0, 1, y], [0, 0, 1]]

newtype Picture a = Picture [a] deriving (Show,Monoid,Functor,Foldable)

transform m p = primTransform m <$> p

line pts = Picture [Line pts]
point (x,y) = Picture [Point (x,y)]

model =  transform (shift 0 100 <> scale 0.6 <> rotate (-pi/6))
      <> transform (shift 0 100 <> scale 0.7)
      <> transform (shift 0 100 <> scale 0.5 <> rotate (pi/6))

primSVG (Point (x,y)) = concat ["<circle"
                                ," cx='", show x, "'"
                                ," cy='", show (400-y), "'"
                                ," r='1'/>"]
primSVG (Line pts) = "<polyline points='" <> points <> "'/>"
  where points = foldMap showPoint pts
        showPoint (x,y) = ' ': show x ++ "," ++ show (400-y)        

toSVG p = "<svg width='400' height='400' fill='blue' stroke='blue'>"
       <> foldMap primSVG p
       <> "</svg>"

tree n = transform (shift 200 0) $ mconcat $ take n $ iterate model $ point (0,100)

main = writeFile "test.html" $ toSVG $ tree 8
