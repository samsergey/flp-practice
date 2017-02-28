{-# LANGUAGE FlexibleInstances #-}
import Data.Monoid
import Data.Semigroup (Min(..), Max(..))
import Data.List.Split (splitOn,chunksOf)
import Data.List (transpose, sort)
import Triscape
import System.Random

times :: (Monoid a, Integral i) => i -> a -> a
0 `times` _ = mempty
1 `times` a = a
2 `times` a = a <> a
n `times` a
  | even n = (n `div` 2) `times` (a <> a)
  | odd n  = (n - 1) `times` a <> a


type Pt = (Float, Float)
  
data Primitive = Point Pt
               | Line [Pt]
               | Circle Pt Float
               | Group [Attribute] [Primitive]
               deriving Show

data Attribute = Color String
               | Fill String
               | LineWidth Float
               | Opacity Float
                deriving (Show, Eq)

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

data Picture = Empty
             | Picture (Box, [Primitive])
             | Transform :$: Picture
             deriving Show

contents :: Picture -> [Primitive]
contents Empty = []
contents (Picture (_, p)) = p
contents p = contents (transform p)

transform :: Picture -> Picture
transform pic = case pic of
  t1 :$: (t2 :$: p) -> transform $ (t1 <> t2) :$: p
  Transform (m,s) :$: p -> app style s . app affine m $ p
  p -> p
  where
    app e m = if m == mempty then id else e m
    style _ Empty = Empty
    style s (Picture (b,p)) = Picture (b, [Group s p]) 

instance Monoid Picture where
  mempty = Empty
  Empty `mappend` p = p
  p `mappend` Empty = p
  Picture p1 `mappend` Picture p2 = Picture (p1 <> p2)
  p1 `mappend` p2 = transform p1 <> transform p2

instance Boxed Picture where
  box Empty = ((0, 0), (0, 0)) 
  box p = let Picture (b,_) = transform p in b

instance Bounded Float where
  minBound = -1/0
  maxBound = 1/0
  
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
    Point (x,y) -> format "<circle fill='blue' stroke='none' cx='_' cy='_' r='1'/>" [short x, short y]
    Line pts -> format "<polyline points='_'/>" [foldMap showPt pts]
    Circle (x,y) r -> format "<circle cx='_' cy='_' r='_'/>" [short x, short y, short r]
    Group s g -> format "<g _>_</g>" [foldMap toSVG s, foldMap toSVG g]
    where 
      showPt (x,y) = format " _,_" [short x, short y]

instance SVG Picture where
  toSVG Empty = "<svg></svg>"
  toSVG p =
    format "<svg width='_' height='_' fill='none' stroke='blue'>_</svg>"
    [show (width p+8), show (height p+8), foldMap toSVG (contents (transform (adjust p)))]

adjust :: Picture -> Picture
adjust p = (scaleY (-1) p) `at` (4,4)

instance SVG Attribute where
  toSVG attr = case attr of
    Color c -> showAttr "stroke" c
    Fill c -> showAttr "fill" c
    LineWidth w -> showAttr "stroke-width" w
    Opacity o -> showAttr "stroke-opacity" o <> showAttr "fill-opacity" o
    where
      showAttr a v = format " _=_" [a, show v]

-- house = walls <> roof
--   where
--     walls = square (0,0) 100
--     roof = triangle (0,100) 100 40

--------------------------------------------------------------------------------

data M a = M ![[a]] | I deriving (Show, Eq)

instance Num a => Monoid (M a) where
  mempty = I
  I `mappend` x = x
  x `mappend` I = x
  M m1 `mappend` M m2 = M [ [ r `dot` c
                            |  c <- transpose m2 ]
                          | r <- m1 ]
    where a `dot` b = sum $ zipWith (*) a b

--------------------------------------------------------------------------------
data Transform = Transform (M Float, [Attribute])
               deriving Show
 
instance Monoid Transform where
  mempty = Transform mempty
  Transform m1 `mappend` Transform m2 = Transform (m1 <> m2)

class Affine a where
  affine :: M Float -> a -> a

instance Affine (Float,Float) where
  affine m (x,y) = let M [[x'],[y'],_] = m <> M [[x],[y],[1]]
                   in (x',y')

instance Affine Primitive where
  affine m p = case p of
    Point pt    -> Point $ affine m pt
    Line pts    -> Line $ (affine m) <$> pts
    Circle pt r -> Circle (affine m pt) (sqrt (x*x + y*y))
      where M [[x],[y],_] = m <> M [[r],[0]]
    Group s g   -> Group s (affine m <$> g)

instance Affine Picture where
  affine m Empty = Empty
  affine m (Picture (_,ps)) = let p' = affine m <$> ps
                              in Picture (foldMap box p', p')
  affine m p = affine m (transform p)

mkAffine :: M Float -> Picture -> Picture
mkAffine m = (Transform (m, mempty) :$:)

shift :: Float -> Float -> Picture -> Picture
shift x y = mkAffine $ M [[1,0,x],[0,1,y],[0,0,1]]

scaleX :: Float -> Picture -> Picture
scaleX a = mkAffine $ M [[a,0,0],[0,1,0],[0,0,1]]

scaleY :: Float -> Picture -> Picture
scaleY a = mkAffine $ M [[1,0,0],[0,a,0],[0,0,1]]

scale :: Float -> Picture -> Picture
scale a = mkAffine $ M [[a,0,0],[0,a,0],[0,0,1]]

shearX a = mkAffine $ M [[1,tan (pi/180*a),0],[0,1,0],[0,0,1]]

rotate :: Float -> Picture -> Picture
rotate a = mkAffine $ M [[c,-s,0],[s,c,0],[0,0,1]]
  where c = cos phi
        s = sin phi
        phi = pi*a/180

at :: Picture -> (Float, Float) -> Picture
p `at` (x,y) = shift (x-x1) (y-y1) p
   where (x1,y1) = (left.lower.corner) p


beside :: Picture -> Picture -> Picture
beside Empty p = p
beside p Empty = p
beside p1 p2 = p1 <> p2 `at` (right.lower.corner) p1

above :: Picture -> Picture -> Picture
above Empty p = p
above p Empty = p
above p1 p2 = p1 `at` (left.upper.corner) p2 <> p2 

row :: Foldable t => t Picture -> Picture
row ps = foldl beside Empty ps

col :: Foldable t => t Picture -> Picture
col ps = foldr above Empty ps


mkStyle :: (t -> Attribute) -> t -> Picture -> Picture
mkStyle k v = (Transform (mempty, [k v]) :$:)

color :: String -> Picture -> Picture
color = mkStyle Color

fill :: String -> Picture -> Picture
fill = mkStyle Fill

lineWidth :: Float -> Picture -> Picture
lineWidth = mkStyle LineWidth

opacity :: Float -> Picture -> Picture
opacity = mkStyle Opacity

rescaleTo a b p = scaleX (a/width p) . scaleY (b/height p) $ p

barChart lst =  row $ rectangle 1 <$> lst

plot f xmin xmax =  color "black" (axes pic) <> (lineWidth 2 pic)
  where dx = (xmax-xmin)/200
        pic = line $ tbl f <$> [xmin,xmin+dx..xmax]
        axes p = line [(x1,0),(x2,0)] <> line [(0,y1),(0,y2)]
          where (x1,y1) = left.lower.corner $ p
                (x2,y2) = right.upper.corner $ p
        tbl f x = (x, f x)
        arrow = fill "black" $ triangle 5 10

sinc 0 = 1
sinc x = sin x / x

short :: (Show a, Floating a) => a -> String
short n = i ++ if (f /= ".0") then (take 3 f) else ""
  where (i,f) = break (=='.') $ show n


polyn k = tail res 
  where res = 0 : 1 : k : zipWith3 (\a b c -> a - 3*b + 3*c) res (tail res) (drop 2 res)

polygonal k n = p
  where M ((p:_):_) = (n `times` M [[3,-3,1],[1,0,0],[0,1,0]]) <> M [[0],[1],[k]]

toPicture :: Triangle -> Picture
toPicture (Triangle p1 p2 p3) = color col . fill col $ line $ project <$> pts
  where
    pts = crop <$> [p1,p2,p3,p1]
    project (x,y,z) = (c*x - s*y,c*y + s*x + z/4)
    crop (x,y,z) = (x,y,max z 0)
    s = sin (pi/6)
    c = cos (pi/6)
    col | all (\(_,_,z) -> z==0) pts = "blue"
        | all (\(_,_,z) -> z<=20) pts = "green"
        | otherwise = "gray"

bay :: [Triangle]
bay = [Triangle (0,0,0) (100,0,0) (0,100,0)
      ,Triangle (100,100,0) (100,0,0) (0,100,0)]

  
main :: IO ()
main = do
  l <- landScape 3 bay <$> randomIO
  writeSVG "test.html" $ scale 3 . opacity 0.5 $ foldMap toPicture l


p = opacity 0.7 $ ((color "blue" $ plot sin (-15) 15) <>
                   (color "green" $ plot (1/) 0.6 15) <>
                   (color "red" $ plot sinc (-15) 15))

p2 = (fill "navy" . opacity 0.6 $ barChart [1,2,4,8,9,3,1])
  <> (fill "orange" . opacity 0.6 $ barChart [12,8,3,2,1,4,8])

sierp 1 = line [(0,0),(3,0),(0,3),(0,0)]
sierp n = let t = sierp (n-1) in t `above` (t `beside` t)

chart p lst = row $ (\i -> col $ replicate i $ p) <$> lst

colors = (rotate 45 . color "black" . fill "red" $ square 50)
  <> (opacity 0.5 . fill "blue" $ circle 30)
  <> (color "orange" . lineWidth 5 $ line [(-40,40),(40,-40)])

mrepeat n f = mconcat . take n . iterate f

fractal model n = mrepeat n (mconcat model) 

tree = fractal model 7 $ line [(0,0), (0,1)]
  where model = [ shift 0 1 . scale 0.6 . rotate (-30)
                , shift 0 1 . scale 0.7 . rotate 5
                , shift 0 1 . scale 0.5 . rotate 45]


circles = fractal model 8 $ circle 1
  where model = [ shift 0 0.75 . scale 0.5 . rotate 60
                , shift 0 (-0.75) . scale 0.5 . rotate (-60) ]
