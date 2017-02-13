{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Data.Monoid
import Data.Semigroup hiding ((<>))
import Data.List.Split (splitOn)
import Data.List (transpose)

times :: (Monoid a, Integral i) => i -> a -> a
0 `times` _ = mempty
1 `times` a = a
2 `times` a = a <> a
n `times` a | even n = (n` div` 2) `times` (2 `times` a)
            | odd n  = a <> (n` div` 2) `times` (2 `times` a)


type Pt = (Float, Float)
  
data Primitive = Point Pt
               | Line [Pt]
               deriving Show

data Picture = Picture (Box, [Primitive])
             deriving Show

box (Picture (((Min x1, Min y1), (Max x2, Max y2)), _)) = ((x1,y1),(x2,y2))
width p = x2 - x1 where ((x1,_),(x2,_)) = box p
height p = y2 - y1 where ((_,y1),(_,y2)) = box p
corner (Picture (((Min x1, Min y1), (Max x2, Max y2)), _)) =
  (((x1,y2),(x2,y2)),((x1,y1),(x2,y1)))
lower = snd
upper = fst
right = snd
left  = fst

contents (Picture (_, p)) = p

type Box = ((Min Float, Min Float), (Max Float, Max Float))

instance Bounded Float where
  minBound = -1000
  maxBound = 1000

instance Monoid Picture where
  mempty = Picture mempty 
  Picture p1 `mappend` Picture p2 = Picture (p1 <> p2)
  
makePicture p = Picture (findBox p, [p])

findBox p = case p of
  Line pts -> foldMap box pts
  Point pt -> box pt
  where box (x,y)= ((Min x, Min y), (Max x, Max y))


square a = makePicture $ Line [(0,0), (a,0), (a,a), (0,a), (0,0)]

rectangle a b = makePicture $ Line [(0,0), (a,0), (a,b), (0,b), (0,0)]

triangle a h = makePicture $ Line [(0,0), (a,0), (a/2,h), (0,0)]

--------------------------------------------------------------------------------

format :: String -> [String] -> String
format s args = mconcat $ zipWith (<>) (splitOn "_" s) (args <> [""])

writeSVG f = writeFile f . toSVG

class SVG a where
  toSVG :: a -> String

instance SVG Primitive where
  toSVG p = case p of
    Point (x,y) -> format "<circle fill='blue' cx='_' cy='_' r='1'/>" [show x, show y]
    Line pts -> format "<polyline points='_'/>" [foldMap showPt pts]
    where 
      showPt (x,y) = format " _,_" [show x, show y]

instance SVG Picture where
  toSVG p =
    format "<svg width='_' height='_' fill='none' stroke='blue'>_</svg>"
    [show (width p), show (height p), foldMap toSVG (contents (adjust p))]

-- house = walls <> roof
--   where
--     walls = square (0,0) 100
--     roof = triangle (0,100) 100 40

--------------------------------------------------------------------------------

data M a = M [[a]] | I deriving Show

instance Num a => Monoid (M a) where
  mempty = I
  I `mappend` x = x
  x `mappend` I = x
  M m1 `mappend` M m2 = M [ [ r `dot` c
                            |  c <- transpose m2 ]
                          | r <- m1 ]
    where a `dot` b = sum $ zipWith (*) a b

--------------------------------------------------------------------------------

class Affine a where
  ($.) :: M Float -> a -> a

instance Affine (Float,Float) where
  m $. (x,y) = let M [[x'],[y'],_] = m <> M [[x],[y],[1]]
                   in (x',y')

instance Affine Primitive where
  m $. p = case p of
    Point pt   -> Point $  m $. pt
    Line pts   -> Line $ (m $.) <$> pts

instance Affine Picture where
  m $. p = Picture (b, ps')
    where
      b = foldMap findBox ps'
      ps' = (m $.) <$> contents p


shift x y = M [[1,0,x],[0,1,y],[0,0,1]]
scaleX a = M [[a,0,0],[0,1,0],[0,0,1]]
scaleY a = M [[1,0,0],[0,a,0],[0,0,1]]
scale a = M [[a,0,0],[0,a,0],[0,0,1]]
rotate a = M [[c,-s,0],[s,c,0],[0,0,1]]
  where c = cos phi
        s = sin phi
        phi = 180*a/pi

p `at` (x,y) = shift (x-x1) (y-y1) $. p
   where (x1,y1) = (left.lower.corner) p

adjust p = (scaleY (-1) $. p) `at` (0,0)

p1 `beside` p2 = p1 <> p2 `at` (right.lower.corner) p1

-- p1 `above` p2 = p1 <> shift (-x1'+x1) (y1-y2') p2
--   where (x1 ,_, y1,_) = box p1
--         (x1',_,_,y2') = box p2

-- row ps = foldr beside mempty ps
-- col ps = foldr above mempty ps
        
main = pure ()
