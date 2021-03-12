module Lab6 where

import Text.Printf
import Data.Monoid
import Data.Semigroup (Min(..), Max(..))
import Data.List (transpose)

import Lab2
import Lab5 (when)

data Pt = Pt Float Float
  deriving (Show, Eq)

getX (Pt x _) = x
getY (Pt _ y) = y

data Primitive = Point Pt
               | Line [Pt]
               | Group [Attribute] [Primitive]
               deriving Show

data Picture = Picture (Box, [Primitive])
  deriving Show

contents :: Picture -> [Primitive]
contents (Picture (_,c)) = c

instance Semigroup Picture where
  Picture p1 <> Picture p2 = Picture (p1 <> p2)
  
instance Monoid Picture where
  mempty = Picture mempty

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

--corner :: Boxed a => a -> ((Pt, Pt),(Pt, Pt))
corner p = (((x1, y1), (x2, y1)),((x1, y2), (x2, y2)))
  where ((Min x1, Min y1),(Max x2, Max y2)) = box p

lower = fst
left = fst
upper = snd
right = snd

instance Boxed a => Boxed [a] where
  box = foldMap box

instance Boxed Pt where
  box (Pt x y) = ((Min x, Min y), (Max x, Max y))

instance Boxed Picture where
  box (Picture (b,_)) = b

instance Boxed Primitive where
  box (Point pt) = box pt
  box (Line pts) = box pts
  box (Group _ ps) = box ps

------------------------------------------------------------

primitive :: Primitive -> Picture
primitive p =  primitives [p]

primitives :: [Primitive] -> Picture
primitives ps =  Picture (box ps, ps)

point :: (Float, Float) -> Picture
point (x,y) = primitive . Point $ Pt x y

line :: [(Float, Float)] -> Picture
line pts = primitive $ Line [Pt x y | (x,y) <- pts]

square :: Float -> Picture
square a = rectangle a a

rectangle :: Float -> Float -> Picture
rectangle a b = line [ (0,0), (a,0), (a, b), (0,b), (0,0)]

polygon :: Int -> Float -> Picture
polygon n a = line [(a*cos t, a*sin t)
                   | t <- [0,2*pi/fromIntegral n..2*pi]] 

circle = polygon 100

------------------------------------------------------------

class SVG a where
  toSVG :: a -> String

instance SVG a => SVG [a]  where
  toSVG = foldMap toSVG

instance SVG Pt where
  toSVG (Pt x y) = printf "%v,%v " x y
    
instance SVG Primitive where
  toSVG p = case p of
    Point (Pt x y) -> printf "<circle rx='%v' ry='%v' r='1'/>" x y
    Line pts -> printf "<polyline points='%s'/>" $ toSVG pts
    Group attr ps -> printf "<g %s>%s</g>" (toSVG attr) (toSVG ps)

instance SVG Picture where
  toSVG p = printf fmt (width p) (height p) prims
    where
      fmt = "<svg width='%f' height='%f' fill='none' stroke='blue'>%s</svg>"
      prims = toSVG (contents p')
      p' = reflect 0 p `at` (0, 0)

writeSVG :: String -> Picture -> IO ()
writeSVG fname = writeFile fname . toSVG

------------------------------------------------------------

data M = M [[Float]] | I deriving Show

outer f l1 l2 = [[f x y | x <- l2 ] | y <- l1]
dot v1 v2 = sum $ zipWith (*) v1 v2

instance Semigroup M where
  m <> I = m
  I <> m = m
  M m1 <> M m2 = M $ outer dot m1 (transpose m2)

instance Monoid M where
  mempty = I

class Affine p where
  affine :: M -> p -> p
  
instance Affine a => Affine [a] where
  affine t ps = affine t <$> ps

rotateM :: Float -> M
rotateM a = M [[cos x, - sin x, 0]
              ,[sin x, cos x, 0]
              ,[0, 0, 1]]
  where x = a / 180 * pi

reflectM :: Float -> M
reflectM a = M [[cos (2*x), sin (2*x), 0]
              ,[sin (2*x), - cos (2*x), 0]
              ,[0, 0, 1]]
  where x = a / 180 * pi
  
translateM :: Float -> Float -> M
translateM x y = M [[1, 0, x]
                   ,[0, 1, y]
                   ,[0, 0, 1]]

scaleM :: Float -> Float -> M
scaleM a b = M [[a, 0, 0]
               ,[0, b, 0]
               ,[0, 0, 1]]

-- масштабирует координату x
scaleX :: Affine a => Float -> a -> a 
scaleX s = affine $ scaleM s 1

 -- масштабирует координату y
scaleY :: Affine a => Float -> a -> a 
scaleY s = affine $ scaleM 1 s

 -- одинаково масштабирует обе координаты
scale :: Affine a => Float -> a -> a 
scale s = affine $ scaleM s s

 -- приводит изображение к указанным размерам 
rescaleTo :: (Boxed a, Affine a) => Float -> Float -> a -> a
rescaleTo a b p = affine (scaleM (a / width p) (b/height p)) p

 -- параллельный перенос изображения
shift :: Affine a => Float -> Float -> a -> a
shift a b = affine $ translateM a b

 -- поворот на угол, задаваемый в градусах вокруг центра координат
rotate :: Affine a => Float -> a -> a
rotate = affine . rotateM

 -- поворот вокруг указанной точки
rotateAt :: Affine a => (Float,Float) -> Float -> a -> a
rotateAt (x, y) a = affine m
  where m = translateM x y <> rotateM a <> translateM (-x) (-y)

reflect :: Affine a => Float -> a -> a
reflect = affine . reflectM


at p (x, y) = shift (x-x') (y-y') p
  where (x', y') = left . lower . corner $ p


instance Affine Pt where
  affine m (Pt x y) = Pt x' y'
    where M [[x'], [y'], [1]] = m <> M [[x], [y], [1]]

 
instance Affine Primitive where
  affine t (Point p) = Point $ affine t p
  affine t (Line pts) = Line $ affine t pts
  affine t (Group attr p) = Group attr $ affine t p

instance Affine Picture where
  affine t p = Picture (box p', p')
    where p' = affine t (contents p)

infixl 5 `beside`
infixr 5 `above`
a `beside` b = a <> b `at` (lower . right . corner) a
a `above` b  = a `at` (upper . left . corner) b <> b
------------------------------------------------------------

data Attribute = LineColor String
               | Fill String
               | LineWidth Float
               | Opacity Float
               deriving (Show, Eq)

instance SVG Attribute where
  toSVG attr = case attr of
    LineColor c -> printf "stroke='%s' " c
    Fill c -> printf "fill='%s' " c
    LineWidth w -> printf "stroke-width='%f' " w
    Opacity o -> printf "fill-opacity='%f' stroke-opacity='%f' " o o

setAttr attr c p = case p of
  Picture (b, [Group a p]) -> Picture (b, [Group (a <> [attr c]) p])
  p -> primitive $ Group [attr c] (contents p)

lineColor = setAttr LineColor
fill = setAttr Fill
color c = setAttr Fill c . setAttr LineColor c
lineWidth = setAttr LineWidth
opacity = setAttr Opacity

mrepeat n f = mconcat . take n . iterate f

------------------------------------------------------------

tree = mrepeat 7 (mconcat model) stem
  where
    stem = line [(0, 0), (0, 1)]
    model = [ shift 0 1 . scale 0.6 . rotate (-30)
            , shift 0 1 . scale 0.7 . rotate 5
            , shift 0 1 . scale 0.5 . rotate 45 ]

pentaflake = (!! 5) $ iterate model $ polygon 5 1
  where
    model = foldMap copy [0,72..288]
    copy a = scale (1/(1+x)) . rotate a . shift 0 x
    x = 2*cos(pi/5)

lineChart :: [Float] -> Picture
lineChart = line . trim . zip [0..]
  where trim l = (0, 0) : l ++ [(fst (last l), 0)]

colorizeWith colors ps = zipWith fill (cycle colors) ps

overlayCharts colors tbl = opacity 0.5 $ mconcat charts
  where charts = colorizeWith colors $ lineChart <$> tbl

------------------------------------------------------------
ssqq n = style . scale 100 $ toPolyline $ toSquare side
    where
      style = lineColor "none" . fill "navy" . lineWidth 0.5
      side = iterate model stem !! n
      stem = line [(0, 0), (1, 0)]
      model = scale 0.25 .
              mconcat [ id
                      , shift 1 0 . rotate 90
                      , shift 1 1
                      , shift 2 1 . scale 2 . rotate (-90)
                      , shift 2 (-1)
                      , shift 3 (-1) . rotate 90
                      , shift 3 0 ]
      toSquare = mconcat [ id
                         , shift 1 0 . rotate 90
                         , shift 1 1 . rotate 180
                         , shift 0 1 . rotate 270 ]

inContents f = primitive . f . contents

toPolyline :: Picture -> Picture
toPolyline p = foldl1 join `inContents` p
  where
    join (Line pts1) (Line pts2)
      | last pts1 == head pts2 = Line (pts1 <> tail pts2)
      | otherwise = Line (pts1 <> pts2)

main = writeSVG "test.html" $ ssqq 2

perimeter (Point _) = mempty
perimeter (Line pts) = mconcat $ zipWith dist pts (tail pts)
  where dist (Pt x y) (Pt a b) = Sum $ sqrt ((x-a)**2 + (y-b)**2)
perimeter (Group _ prs) = foldMap perimeter  prs

pointNum (Point _) = Sum 1
pointNum (Line pts) = Sum $ length pts
pointNum (Group _ ps) = foldMap pointNum ps

------------------------------------------------------------
                        
coch = LSystem f  s
    where
      s = "F+F-F-F.F+F+F-F"
      f x = case x of
              'F' -> s
              x -> pure x

data LSystem = LSystem { rewrite :: (Char -> String)
                       , start :: String }
             
runLS (LSystem r s) = iterate (>>= r) s 

add (Pt x1 y1) (Pt x2 y2) = Pt (x1+x2) (y1+y2)
                      
drawLS l s n = shift . compose . iterprete $ string
    where
      string = runLS l !! n
      shift = scanl add (Pt 0 0)
      compose = scanl (flip ($)) s
      iterprete = foldMap $
                  \c -> case c of
                          'F' -> mempty
                          '.' -> [id]
                          '+' -> [rotate 90]
                          '-' -> [rotate (-90)]
                           
