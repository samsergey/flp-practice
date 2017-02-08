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
  M m1 `mappend` M m2 = M [ [ r `dot` c |  c <- transpose m2 ] |r <- m1 ]
    where a `dot` b = sum $ zipWith (*) a b

--------------------------------------------------------------------------------

fib 0 = 1
fib n = a
  where M ((a:_):_) = n `times` M [[1,1,0],[1,0,0],[0,1,0]]

fibs = 1 : scanl' (+) 1 fibs

--------------------------------------------------------------------------------

data Picture = Picture [Primitive] deriving Show

data Primitive = Line [(Float, Float)] deriving Show

line pts = Picture $ [Line pts]

primTransform m (Line pts) = Line $ trans <$> pts
  where trans (x,y) = let M [[x'],[y'],_] = m <> M [[x],[y],[1]] in (x',y')

transform m (Picture ps) = Picture $ primTransform m <$> ps

rotate a = M [[c, -s, 0]
             ,[s,  c, 0]
             ,[0,  0, 1]]
  where c = cos a
        s = sin a
        
scale s = M [[s, 0, 0]
            ,[0, s, 0]
            ,[0, 0, 1]]

shift x y = M [[1, 0, x]
              ,[0, 1, y]
              ,[0, 0, 1]]

instance Monoid Picture where
  mempty = Picture []
  Picture p1 `mappend` Picture p2 = Picture (p1 <> p2)

model :: Picture -> Picture
model =  transform (shift 0 100 <> scale 0.6 <> rotate (-pi/6))
      <> transform (shift 0 100 <> scale 0.7)
      <> transform (shift 0 100 <> scale 0.5 <> rotate (pi/6))
                 
tree n = transform (shift 200 0) $ fold $ take n $ iterate model $ line [(0,0),(0,100)]         

primSVG (Line pts) = "<polyline fill='none' stroke='blue' points='" <> points <> "'/>"
  where points = foldMap point pts
        point (x,y) = ' ': show x ++ "," ++ show (400-y)        

toSVG (Picture ps) = "<svg width='400' height='400'>" <> foldMap primSVG ps <> "</svg>"

main = writeFile "test.html" $ toSVG $ tree 10
