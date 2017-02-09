import Data.Monoid
import Data.Semigroup (Min(..),Max(..))
import Data.List
import Data.List.Split
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
type Pt = (Float, Float)

data Primitive = Point Pt
               | Line [Pt]
               | Circle Pt Float
  deriving Show

class SVG a where
  toSVG :: a -> String

instance SVG Primitive where

  toSVG (Point (x,y)) = format "<circle fill='blue' cx='_' cy='_' r='1'/>" [show x, show y]

  toSVG (Circle (x,y) r) = format "<circle cx='_' cy='_' r='_'/>" [show x, show y, show r]

  toSVG (Line pts) = format "<polyline points='_'/>" [points]
    where points = foldMap showPt pts
          showPt (x,y) = format " _,_" [show x, show y]

format :: String -> [String] -> String
format s args = mconcat $ zipWith (<>) (splitOn "_" s) (args <> [""])

type BoundingBox = (Min Float, Max Float, Min Float, Max Float)

instance Bounded Float where
  minBound = 0
  maxBound = 1000

data Picture = Picture BoundingBox [Primitive] deriving Show

instance SVG Picture where
  toSVG (Picture (Min x1, Max x2, Min y1, Max y2) ps) =
    format "<svg width='_' height='_' fill='none' stroke='blue'>_</svg>"
    [show (x2+5), show (y2+5), foldMap toSVG ps]

line pts = Picture (foldMap bb pts) [Line pts]
  where bb (x,y)= (Min x, Max x, Min y, Max y)

point (x,y) = Picture (Min x, Max x, Min y, Max y) [Point (x,y)]

circle (x,y) r = Picture (Min (x-r), Max (x+r), Min (y-r), Max (y+r))
                 [Circle (x,y) r]

polygon (x,y) n r = line $ [(x + r*cos a, y + r*sin a)
                           | a <- [0,2*pi/fromIntegral n..2*pi]]

instance Monoid Picture where
  mempty = Picture mempty mempty
  Picture bb1 p1 `mappend` Picture bb2 p2 = Picture (bb1 <> bb2) (p1 <> p2)


-- --------------------------------------------------------------------------------

-- primTransform m p = case p of
--   Point pt   -> Point $ trans pt
--   Line pts   -> Line $ trans <$> pts
--   Group s ps -> Group s $ primTransform m <$> ps 
--   where trans (x,y) = let M [[x'],[y'],_] = m <> M [[x],[y],[1]] in (x',y')

-- rotate a = M [[c, -s, 0], [s,  c, 0], [0,  0, 1]]
--   where c = cos a
--         s = sin a
        
-- scale s = M [[s, 0, 0], [0, s, 0], [0, 0, 1]]

-- shift x y = M [[1, 0, x], [0, 1, y], [0, 0, 1]]


-- transform m p = primTransform m <$> p

-- line pts = Picture [Line pts]
-- dot (x,y) = Picture [Point (x,y)]

-- model =  transform (shift 0 100 <> scale 0.6 <> rotate (-pi/6))
--       <> transform (shift 0 100 <> scale 0.7)
--       <> transform (shift 0 100 <> scale 0.5 <> rotate (pi/6))



-- tree n = transform (shift 200 0) $ mconcat $ take n $ iterate model $ point (0,100)

-- main = writeFile "test.html" $ toSVG $ tree 8
