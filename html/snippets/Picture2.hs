import Data.Monoid
import Data.Semigroup hiding ((<>))
import Data.List.Split (splitOn)
import Data.List (transpose)

type Pt = (Float, Float)
  
data Primitive = Point Pt
               | Line [Pt]
               | Group String [Primitive]
               deriving Show

data Picture = Picture Box [Primitive] deriving Show

box (Picture ((Min x1, Min y1), (Max x2, Max y2)) _) = ((x1,y1),(x2,y2))
width p = x2 - x1 where ((x1,_),(x2,_)) = box p
height p = y2 - y1 where ((_,y1),(_,y2)) = box p
contents (Picture _ p) = p


type Box = ((Min Float, Min Float), (Max Float, Max Float))

instance Bounded Float where
  minBound = -1000
  maxBound = 1000

instance Monoid Picture where
  mempty = Picture mempty mempty
  Picture b1 p1 `mappend` Picture b2 p2 = Picture (b1 <> b2) (p1 ++ p2)
  
makePicture p = Picture (findBox p) [p]

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

data Trans = Id
           | Trans :+: Trans
           | Affine (M Float)
           | Attrib (String) deriving Show

instance Monoid Trans where
  mempty = Id
  Id `mappend` t = t
  t `mappend` Id = t
  Affine m1 `mappend` Affine m2 = Affine (m1 <> m2)
  Affine m1 `mappend` (Affine m2 :+: t) = Affine (m1 <> m2) :+: t
  Attrib s1 `mappend` Attrib s2 = Attrib (s1 <> s2)
  Attrib s1 `mappend` (Attrib s2 :+: t) = Attrib (s1 <> s2) :+: t
  t1 :+: t2 `mappend` ts = t1 :+: (t2 :+: ts)
  t `mappend` ts = t :+: ts

(%) :: Trans -> Picture -> Picture
Id % p = p
(t1 :+: ts) % p = t1 % (ts % p)
Attrib s % (Picture b [Group a ps]) = Picture b [Group (s <> a) ps]
Attrib s % (Picture b p) = Picture b [Group s p]
Affine m % (Picture b p) = Picture (foldMap findBox p') p' 
  where
    p' = transform <$> p
    transform p = case p of
      Point pt   -> Point $ trans pt
      Line pts   -> Line $ trans <$> pts
      Group a ps -> Group a $ transform <$> ps
    trans (x,y) = let M [[x'],[y'],_] = m <> M [[x],[y],[1]] in (x',y')

shift x y = Affine $ M [[1,0,x],[0,1,y],[0,0,1]]
rotate a = Affine $ M [[c,-s,0],[s,c,0],[0,0,1]]
  where c = cos phi
        s = sin phi
        phi = 180*a/pi
        
color c = Attrib $ format " stroke='_'" [c]

main = pure ()
