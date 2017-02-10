import Data.Monoid
import Data.Semigroup (Min(..),Max(..))
import Data.List
import Data.List.Split
import Data.Foldable
import Control.Arrow

when :: Monoid t1 => (t -> Bool) -> t1 -> t -> t1
when p m x = if p x then m else mempty


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
  deriving Show

class SVG a where
  toSVG :: a -> String

instance SVG Primitive where

  toSVG (Point (x,y)) = format "<circle fill='blue' cx='_' cy='_' r='1'/>" [show x, show y]

  toSVG (Line pts) = format "<polyline points='_'/>" [points]
    where points = foldMap showPt pts
          showPt (x,y) = format " _,_" [show x, show y]

format :: String -> [String] -> String
format s args = mconcat $ zipWith (<>) (splitOn "_" s) (args <> [""])

type Box = (Min Float, Max Float, Min Float, Max Float)

instance Bounded Float where
  minBound = 0
  maxBound = 1000

data Picture = Picture Box [Primitive] deriving Show

box (Picture (Min x1, Max x2, Min y1, Max y2) _) = (x1,x2,y1,y2)

instance SVG Picture where
  toSVG (Picture (Min x1, Max x2, Min y1, Max y2) ps) =
    format "<svg width='_' height='_' fill='none' stroke='blue'>_</svg>"
    [show (x2-x1+8), show (y2-y1+8), foldMap toSVG ps']
    where
      ps' = affine (scaleT 1 (-1) <> shiftT (-x1+4) (-y2-4)) <$> ps

getBox p = case p of
  Line pts -> foldMap box pts
  Point pt -> box pt
  where box (x,y)= (Min x, Max x, Min y, Max y)

makePicture p = Picture (getBox p) [p]

line pts = makePicture $ Line pts

point pt = makePicture $ Point pt

circle r = polygon 20 r

polygon n r = line $ [(r*sin a, r*cos a)
                     | a <- [0,2*pi/fromIntegral n..2*pi]]

square a = line [(0,0),(0,a),(a,a),(a,0),(0,0)]

rectangle a b = scale 1 (b/a) $ square a

instance Monoid Picture where
  mempty = Picture mempty mempty
  Picture bb1 p1 `mappend` Picture bb2 p2 = Picture (bb1 <> bb2) (p1 <> p2)

affine m p = case p of
  Point pt   -> Point $ trans pt
  Line pts   -> Line $ trans <$> pts
  where trans (x,y) = let M [[x'],[y'],_] = m <> M [[x],[y],[1]] in (x',y')

rotateT a = M [[c, -s, 0], [s,  c, 0], [0,  0, 1]]
  where c = cos a
        s = sin a
        
scaleT sx sy = M [[sx, 0, 0], [0, sy, 0], [0, 0, 1]]

shiftT x y = M [[1, 0, x], [0, 1, y], [0, 0, 1]]

transform m (Picture bb p) = Picture (foldMap getBox p') p' 
  where p' = affine m <$> p

shift x y = transform (shiftT x y)
scale x y = transform (scaleT x y)
rotate a = transform (rotateT a)
p `at` (x,y) = shift x y p

p1 `beside` p2 = p1 <> shift (-x1'+x2) (y1-y1') p2
  where (_  ,x2,y1 ,_) = box p1
        (x1',_ ,y1',_) = box p2

p1 `above` p2 = p1 <> shift (-x1'+x1) (y1-y2') p2
  where (x1 ,_, y1,_) = box p1
        (x1',_,_,y2') = box p2

row, col :: [Picture] -> Picture
row = foldr beside mempty
col = foldr above mempty

model =  transform (shiftT 0 100 <> scaleT 0.6 0.6 <> rotateT (-pi/6))
      <> transform (shiftT 0 100 <> scaleT 0.7 0.7)
      <> transform (shiftT 0 100 <> scaleT 0.5 0.5 <> rotateT (pi/6))

tree n = mconcat $ take n $ iterate model $ line [(0,0), (0,100)]

sierp 1 = polygon 3 4
sierp n = let t = sierp (n-1) in t `above` (t `beside` t)

writeSVG f = writeFile f . toSVG

main = writeFile "test.html" $ toSVG $ tree 8

wheel = mconcat $ take 48 $ iterate (rotate (pi/24)) $ square 100 `at` (50,50)

chart p lst = row $ (\i -> col $ replicate i $ p) <$> lst
barChart a lst =  row $ rectangle a <$> lst
