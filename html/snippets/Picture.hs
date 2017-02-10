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

data Attribute = Color String
               | Fill String
               | LineWidth Float
               | Opacity Float
  deriving Show

instance SVG Attribute where
  toSVG attr = case attr of
    Color c -> showAttr "stroke" c
    Fill c -> showAttr "fill" c
    LineWidth w -> showAttr "stroke-width" (show w)
    Opacity o -> showAttr "stroke-opacity" (show o) <> showAttr "fill-opacity" (show o)
    where showAttr a v = format " _='_'" [a, v]

data Primitive = Point Pt
               | Line [Pt]
               | Group Attribute [Primitive]
  deriving Show

class SVG a where
  toSVG :: a -> String

format :: String -> [String] -> String
format s args = mconcat $ zipWith (<>) (splitOn "_" s) (args <> [""])

instance SVG Primitive where

  toSVG (Point (x,y)) = format "<circle fill='blue' cx='_' cy='_' r='1'/>" [show x, show y]

  toSVG (Line pts) = format "<polyline points='_'/>" [points]
    where points = foldMap showPt pts
          showPt (x,y) = format " _,_" [show x, show y]

  toSVG (Group attr ps) = format "<g _>_</g>" [toSVG attr, foldMap toSVG ps]


type Box = (Min Float, Max Float, Min Float, Max Float)

instance Bounded Float where
  minBound = 0
  maxBound = 1000

data Picture = Picture Box [Primitive] deriving Show


box (Picture (Min x1, Max x2, Min y1, Max y2) _) = (x1,x2,y1,y2)
width p = x2 - x1 where (x1,x2,_,_) = box p
height p = y2 - y1 where (_,_,y1,y2) = box p
contents (Picture _ p) = p

instance SVG Picture where
  toSVG p = format "<svg width='_' height='_' fill='none' stroke='blue'>_</svg>"
            [show (width p'+8), show (height p'+8), foldMap toSVG (contents p')]
    where p' = adjust p

            
adjust p = transform (scaleT 1 (-1) <> shiftT (-x1+4) (-y2-4)) p
  where (x1,_,_,y2) = box p

getBox p = case p of
  Line pts -> foldMap box pts
  Point pt -> box pt
  Group _ ps -> foldMap getBox ps
  where box (x,y)= (Min x, Max x, Min y, Max y)

makePicture p = Picture (getBox p) [p]

line pts = makePicture $ Line pts

point pt = makePicture $ Point pt

circle r = polygon 20 r

polygon n r = line $ [(r*sin a, r*cos a)
                     | a <- [0,2*pi/fromIntegral n..2*pi]]

square a = line [(0,0),(0,a),(a,a),(a,0),(0,0)]

rectangle a b = scale 1 (b/a) $ square a

setAttr a (Picture b p) = Picture b [Group a p]

color c = setAttr (Color c)
fill c = setAttr (Fill c)
lineWidth w = setAttr (LineWidth w)
opacity o = setAttr (Opacity o)

instance Monoid Picture where
  mempty = Picture mempty mempty
  Picture bb1 p1 `mappend` Picture bb2 p2 = Picture (bb1 <> bb2) (p1 <> p2)

affine m p = case p of
  Point pt   -> Point $ trans pt
  Line pts   -> Line $ trans <$> pts
  Group a ps -> Group a $ affine m <$> ps
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

row ps = foldr beside mempty ps
col ps = foldr above mempty ps

tree = fractal model 7 $ line [(0,0), (0,100)]
  where model = [ shiftT 0 100 <> scaleT 0.6 0.6 <> rotateT (-pi/6)
                , shiftT 0 100 <> scaleT 0.7 0.7
                , shiftT 0 100 <> scaleT 0.5 0.5 <> rotateT (pi/6)]

circles = fractal model 8 $ circle 150
  where model = [ shiftT 0 75 <> scaleT 0.5 0.5 <> rotateT (pi/3)
                , shiftT 0 (-75) <> scaleT 0.5 0.5 <> rotateT (-pi/3) ]

pithagor = fractal model 10 $ square 100
  where model = [ shiftT 0 100 <> scaleT s s <> rotateT (pi/4)
                , shiftT 100 100 <> scaleT s s <> rotateT (-pi/4)]
        s = 1/sqrt 2

pentaflake = fractal model 5 $ polygon 5 50
  where model =  map copy [0,72..288]
        copy a = scaleT s s <> rotateT (pi*a/180) <> shiftT 0 x
        x = 2*cos(pi/5)
        s = 1/(1+x)

fractal :: Foldable t => t (M Float) -> Int -> Picture -> Picture
fractal model n = mconcat . take n . iterate (foldMap transform model) 

sierp 1 = polygon 3 4
sierp n = let t = sierp (n-1) in t `above` (t `beside` t)

writeSVG f = writeFile f . toSVG
writeCanvas f = writeFile f . toCanvas

main = writeFile "test.html" $ toSVG $ tree

wheel = mconcat $ take 48 $ iterate (rotate (pi/24)) $ square 50 `at` (25,25)

chart p lst = row $ (\i -> col $ replicate i $ p) <$> lst
barChart a lst =  row $ rectangle a <$> lst

row1 = row $ circle <$> [5,10,20,40,20,10,5]
chart1 = chart (square 10) [1,2,1,2,3,2,1,4,2,3,6,5,4,3,4,3,1,2,1]
chart2 = scale 1 10 $ barChart 10 [1,2,3,2,3,4,5,4,3,2]

charts = opacity 0.5 (fill "blue" chart1 <> fill "red" chart2)
  where 
    chart1 = barChart 10 [10,20,30,20,30,40,50,40,30,20]
    chart2 = barChart 10 [30,10,35,50,10,45,60,20,10,5]

class Canvas a where
  toCanvas :: a -> String

instance Canvas Primitive where
  toCanvas p = unlines $ case p of
    Line [] -> mempty
    Line (pt:pts) -> path $ moveTo pt <> foldMap lineTo pts
    Point pt -> path $ arc pt 
    Group attr ps -> group $ toCanvas attr <> foldMap toCanvas ps
    where
      path p = command "beginPath" [] <> p <> command "stroke" []
      moveTo (x,y) = command "moveTo" [show x, show y]
      lineTo (x,y) = command "lineTo" [show x, show y]
      arc (x,y) = command "arc" [show x, show y, "2", "0", "Math.Pi"]
      group g = [ "ss = img.strokeStyle; sf = img.fillStyle; os = img.globalOpacity;"
                , g
                ,  "img.strokeStyle = ss; img.fillStyle = sf; img.globalOpacity = os;"]

instance Canvas Picture where
  toCanvas p = header <> start <> foldMap toCanvas (contents p') <> finish
    where
      p' = adjust p
      header = format "<canvas id='img' width='_' height='_'></canvas>\n" [show $ width p+18, show $ height p+18]
      start = "<script>\nvar ctx = img.getContext('2d');\nvar ss,fs,os;\n"
      finish = "</script>"

instance Canvas Attribute where
  toCanvas attr = case attr of
    Color c -> showAttr "strokeStyle" (show c)
    Fill c -> showAttr "fillStyle" (show c)
    LineWidth w -> showAttr "lineWidth" (show w)
    Opacity o -> showAttr "globalAlpha" (show o)
    where showAttr a v = format "ctx._ = _;" [a, v]
  

command :: String -> [String] -> [String]
command cmd args = ["ctx." <> cmd <> "(" <> (drop 1 $ foldMap (',':) args) <> ");"]
