import Text.Printf
import Data.Monoid
import Data.Semigroup (Min(..), Max(..))

data Pt = Pt Float Float
  deriving Show
  
data Primitive = Point Pt
               | Line [Pt]
               deriving Show

data Picture = Empty
             | Picture (Box, [Primitive])
             deriving Show

contents (Picture (_,c)) = c
contents Empty = []

instance Semigroup Picture where
  Empty <> p = p
  p <> Empty = p
  Picture p1 <> Picture p2 = Picture (p1 <> p2)
  
instance Monoid Picture where
  mempty = Empty

------------------------------------------------------------

type Box = ( (Min Float, Min Float)
           , (Max Float, Max Float) )

instance Bounded Float where
  minBound = -1/0
  maxBound = 1/0

class Boxed a where
  box :: a -> Box

width :: Boxed a => a -> Float
width p = x2 - x1
  where ((Min x1,_),(Max x2,_)) = box p

height :: Boxed a => a -> Float
height p = y2 - y1
  where ((_,Min y1),(_,Max y2)) = box p

instance Boxed Pt where
  box (Pt x y) = ((Min x, Min y), (Max x, Max y))

instance Boxed Picture where
  box Empty = ((Min 0, Min 0), (Max 0, Max 0))
  box (Picture (b,_)) = b

instance Boxed Primitive where
  box (Point pt) = box pt
  box (Line pts) = foldMap box pts

------------------------------------------------------------

primitive :: Primitive -> Picture
primitive p =  Picture (box p, [p])

point :: Pt -> Picture
point = primitive . Point

line :: [Pt] -> Picture
line = primitive . Line

square :: (Float, Float) -> Float -> Picture
square pt a = rectangle pt a a

rectangle :: (Float, Float) -> Float -> Float -> Picture
rectangle (x, y) a b = line [ Pt x y, Pt (x+a) y
                            , Pt (x+a) (y+b), Pt x (y+b)
                            , Pt x y]

polygon :: (Float, Float) -> Int -> Float -> Picture
polygon (x, y) n a = line [Pt (x+a*cos t) (x+a*sin t)
                          | t <- [0,2*pi/fromIntegral n..2*pi]] 

------------------------------------------------------------

class SVG a where
  toSVG :: a -> String

instance SVG Pt where
  toSVG (Pt x y) = printf "%v,%v " x y
    
instance SVG Primitive where
  toSVG p = case p of
    Point (Pt x y) -> printf "<circle rx='%v' ry='%v' r='1'/>" x y
    Line pts -> printf "<polyline points='%s'/>" $ foldMap toSVG pts

instance SVG Picture where
  toSVG p = printf fmt (width p) (height p) prims
    where
      fmt = "<svg width='%v' height='%v' fill='none' stroke='blue'>%s</svg>"
      prims = foldMap toSVG (contents p)

writeSVG :: SVG a => String -> a -> IO ()
writeSVG fname = writeFile fname . toSVG

------------------------------------------------------------

class Trans p where
  transform :: 
