{-# LANGUAGE FlexibleInstances #-}
import Data.Monoid
import Data.Semigroup (Min(..), Max(..))
import Data.List.Split (splitOn)
import Data.List (transpose, sort)
import qualified Data.Map as M

times :: (Monoid a, Integral i) => i -> a -> a
0 `times` _ = mempty
1 `times` a = a
2 `times` a = a <> a
n `times` a | even n = (n` div` 2) `times` (2 `times` a)
            | odd n  = a <> (n` div` 2) `times` (2 `times` a)


type Pt = (Float, Float)
  
data Primitive = Point Pt
               | Line [Pt]
               | Circle Pt Float
               | Group Attributes [Primitive]
               deriving Show

type Attributes  = M.Map String String

type Box = ((Min Float, Min Float), (Max Float, Max Float))

class Boxed a where
  box :: a -> Box

  width :: a -> Float
  width p = x2 - x1 where ((Min x1,_),(Max x2,_)) = box p

  height :: a -> Float
  height p = y2 - y1 where ((_,Min y1),(_,Max y2)) = box p

  corner :: a -> ((Pt,Pt),(Pt,Pt))
  corner p = (((x1,y2),(x2,y2)),((x1,y1),(x2,y1)))
    where ((Min x1,Min y1),(Max x2, Max y2)) = box p

lower = snd
upper = fst
right = snd
left  = fst

instance Boxed Primitive where
  box p = case p of
    Line pts -> foldMap box pts
    Point pt -> box pt
    Circle (x,y) r -> box $ Line [(x-r,y-r),(x+r,y+r)]
    Group _ g -> foldMap box g

instance Boxed Pt where
  box (x,y)= ((Min x, Min y), (Max x, Max y))
  width _ = 0
  height _ = 0

data Picture = Picture (Box, [Primitive])
             | Transform :$: Picture
             deriving Show

contents :: Picture -> [Primitive]
contents (Picture (_, p)) = p
contents p = contents (transform p)

transform :: Picture -> Picture
transform pic = case pic of
  (t1 :+: t2) :$: p -> transform $ t1 :$: transform (t2 :$: p)
  t1 :$: (t2 :$: p) -> transform $ (t1 <> t2) :$: p
  Picture p -> Picture p
  Affine m :$: Picture p -> affine m (Picture p)
  Style s :$: Picture (b,p) -> Picture (b, [Group s p])

instance Monoid Picture where
  mempty = Picture mempty 
  Picture p1 `mappend` Picture p2 = Picture (p1 <> p2)
  p1 `mappend` p2 = transform p1 <> transform p2

instance Boxed Picture where
  box p = b where Picture (b,_) = transform p

instance Bounded Float where
  minBound = -1000
  maxBound = 1000
  
primitive :: Primitive -> Picture
primitive p = Picture (box p, [p])

point :: Pt -> Picture
point pt = primitive $ Point pt

line :: [Pt] -> Picture
line pts = primitive $ Line pts

circle :: Float -> Picture
circle r = primitive $ Circle (0,0) r

square :: Float -> Picture
square a = primitive $ Line [(0,0), (a,0), (a,a), (0,a), (0,0)]

rectangle :: Float -> Float -> Picture
rectangle a b = primitive $ Line [(0,0), (a,0), (a,b), (0,b), (0,0)]

triangle :: Float -> Float -> Picture
triangle a h = primitive $ Line [(0,0), (a,0), (a/2,h), (0,0)]

--------------------------------------------------------------------------------

format :: String -> [String] -> String
format s args = mconcat $ zipWith (<>) (splitOn "_" s) (args <> [""])

writeSVG :: SVG a => FilePath -> a -> IO ()
writeSVG f = writeFile f . toSVG

class SVG a where
  toSVG :: a -> String

instance SVG Primitive where
  toSVG p = case p of
    Point (x,y) -> format "<circle fill='blue' stroke='none' cx='_' cy='_' r='1'/>" [show x, show y]
    Line pts -> format "<polyline points='_'/>" [foldMap showPt pts]
    Circle (x,y) r -> format "<circle cx='_' cy='_' r='_'/>" [show x, show y, show r]
    Group s g -> format "<g _>_</g>" [toSVG s, foldMap toSVG g]
    where 
      showPt (x,y) = format " _,_" [show x, show y]

instance SVG Picture where
  toSVG p =
    format "<svg width='_' height='_' fill='none' stroke='blue'>_</svg>"
    [show (width p), show (height p), foldMap toSVG (contents (transform (adjust p)))]

instance SVG Attributes where
  toSVG attr = foldMap (\(k,v) -> format " _='_'" [k, v]) $ M.toList attr


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
data Transform = Affine (M Float)
               | Style Attributes
               | Transform :+: Transform
               deriving Show

infixr 4 :+:
infixr 4 :$:
  
instance Monoid Transform where
  mempty = Affine mempty
  Affine m1 `mappend` Affine m2 = Affine (m1 <> m2)
  Style s1 `mappend` Style s2 = Style (s1 <> s2)
  t `mappend` Style s = Style s :+: t
  t1 `mappend` t2 = t1 :+: t2

class Affine a where
  affine :: M Float -> a -> a

instance Affine (Float,Float) where
  affine m (x,y) = let M [[x'],[y'],_] = m <> M [[x],[y],[1]]
                   in (x',y')

instance Affine Primitive where
  affine m p = case p of
    Point pt    -> Point $ affine m pt
    Line pts    -> Line $ (affine m) <$> pts
    Circle pt r -> Circle (affine m pt) r
    Group s g   -> Group s (affine m <$> g)

instance Affine Picture where
  affine m (Picture (_,ps)) = let p' = affine m <$> ps
                              in Picture (foldMap box p', p')
  affine m p = affine m (transform p)


shift :: Float -> Float -> Picture -> Picture
shift x y = (Affine (M [[1,0,x],[0,1,y],[0,0,1]]) :$:)

scaleX :: Float -> Picture -> Picture
scaleX a = (Affine (M [[a,0,0],[0,1,0],[0,0,1]]) :$:)

scaleY :: Float -> Picture -> Picture
scaleY a = (Affine (M [[1,0,0],[0,a,0],[0,0,1]]) :$:)

scale :: Float -> Picture -> Picture
scale a = (Affine (M [[a,0,0],[0,a,0],[0,0,1]]) :$:)

rotate :: Float -> Picture -> Picture
rotate a = (Affine (M [[c,-s,0],[s,c,0],[0,0,1]]) :$:)
  where c = cos phi
        s = sin phi
        phi = pi*a/180

at :: Picture -> (Float, Float) -> Picture
p `at` (x,y) = shift (x-x1) (y-y1) p
   where (x1,y1) = (left.lower.corner) p

adjust :: Picture -> Picture
adjust p = (scaleY (-1) p) `at` (0,0)

beside :: Picture -> Picture -> Picture
beside p1 p2 = p1 <> p2 `at` (right.lower.corner) p1

above :: Picture -> Picture -> Picture
above p1 p2 = p1 `at` (left.upper.corner) p2 <> p2 

row :: Foldable t => t Picture -> Picture
row ps = foldl beside mempty ps

col :: Foldable t => t Picture -> Picture
col ps = foldr above mempty ps


makeAttr :: String -> String -> Picture -> Picture
makeAttr k v = (Style (M.singleton k v) :$:)

color :: String -> Picture -> Picture
color = makeAttr "stroke"

fill :: String -> Picture -> Picture
fill = makeAttr "fill"

lineWidth :: Show a => a -> Picture -> Picture
lineWidth w = makeAttr "lineWidth" $ show w

opacity :: Show a => a -> Picture -> Picture
opacity a = (makeAttr "stroke-opacity" <> makeAttr "fill-opacity") (show a) 

main :: IO ()
main = pure ()
