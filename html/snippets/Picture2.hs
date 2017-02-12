{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Data.Monoid
import Data.Semigroup hiding ((<>))
import Data.List.Split (splitOn)
import Data.List (transpose)

type Pt = (Float, Float)
  
data Primitive = Point Pt
               | Line [Pt]
               deriving Show

data Picture = Picture (Box, [Primitive])
             deriving Show

box (Picture (((Min x1, Min y1), (Max x2, Max y2)), _)) = ((x1,y1),(x2,y2))
width p = x2 - x1 where ((x1,_),(x2,_)) = box p
height p = y2 - y1 where ((_,y1),(_,y2)) = box p
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

square :: Pt -> Float -> Picture
square (x,y) a = makePicture $ Line [(x,y), (x+a,y), (x+a,y+a), (x,y+a), (x,y)]
rectangle :: Pt -> Float -> Float -> Picture
rectangle (x,y) a b = makePicture $ Line [(x,y), (x+a,y), (x+a,y+b), (x,y+b), (x,y)]
triangle :: Pt -> Float -> Float -> Picture
triangle (x,y) a h = makePicture $ Line [(x,y), (x+a,y), (x+a/2,y+h), (x,y)]

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
    [show (width p), show (height p), foldMap toSVG (contents p)]

house = walls <> roof
  where
    walls = square (0,0) 100
    roof = triangle (0,100) 100 40

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

data Tr = Id | Affine (M Float) deriving Show

instance Monoid Tr where
  mempty = Id
  Id `mappend` x = x
  x `mappend` Id = x
  Affine m1 `mappend` Affine m2 = Affine $ m1 <> m2

Id .$ p = p
Affine m .$ p = affine m p

class Affine a where
  affine :: (M Float) -> a -> a

instance Affine (Float,Float) where
  affine m (x,y) = let M [[x'],[y'],_] = m <> M [[x],[y],[1]]
                      in (x',y')

instance Affine Primitive where
  affine m p = case p of
    Point pt   -> Point $ affine m pt
    Line pts   -> Line $ affine m <$> pts

instance Affine Picture where
  affine m p = Picture (b, ps')
    where
      b = foldMap findBox ps'
      ps' = affine m <$> contents p


shift x y = Affine $ M [[1,0,x],[0,1,y],[0,0,1]]
rotate a = Affine $ M [[c,-s,0],[s,c,0],[0,0,1]]
  where c = cos phi
        s = sin phi
        phi = 180*a/pi
        
main = pure ()
