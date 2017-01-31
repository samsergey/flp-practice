{-# language TypeApplications #-}
import Control.Applicative

type NumFunc = Double -> Double
type Point = (Double, Double)

tab :: NumFunc -> Double -> Point
tab f x = (x, f x)

type Range = (Double, Double)
type Table = [Point]

tabulate :: NumFunc -> Range -> Int -> Table
tabulate f (x1,x2) n = tab f <$> [x1, x1+h .. x2]
  where h = (x2-x1) / fromIntegral n
 
house :: String
house = "<svg width='400' height='300'>" ++ walls ++ roof ++"</svg>"
  where
    walls = "<rect x='10' y='240' width='50' height='50' fill='brown'/>"
    roof = "<polyline points='10,240 35,200 60,240' fill='none' stroke='black'/>"

polyline :: Table -> String
polyline pts = "<polyline points='" ++ points ++ "' fill='none' stroke='blue'/>"
  where
    points = unwords $ (\(x,y) -> show x ++ "," ++ show y) <$> pts

scale :: Range -> Range -> Double -> Double
scale (a,b) (c,d) x = (d-c)/(b-a)*(x-a)+c

sinPlot :: String
sinPlot = "<svg width=400 height=300>"++ polyline tbl ++"</svg>"
  where
    tbl = tabulate (sY . sin . sX) (0, 400) 100
    sX = scale (0, 400) (0, 3*pi)
    sY = scale (-1, 1) (300, 0)

plot :: NumFunc -> (Range, Range) -> Int -> String
plot f (xRange, yRange) n = "<svg width=400 height=300>"++ polyline tbl ++"</svg>"
  where
    tbl = tabulate (sY . f . sX) (0, 400) n
    sX = scale (0, 400) xRange
    sY = scale yRange (300, 0)

showN :: Int -> Double -> String
showN n x = show $ roundof (x*10^n)/10^n
  where
    roundof = fromIntegral . round

sinc :: Double -> Double
sinc x
  | x == 0 = 1
  | otherwise = sin x / x

bisection :: Alternative f
          => (Double -> Double) -> Double -> Double
          -> f Double
bisection f a b
  | f a * f b > 0 = empty
  | abs (b - a) / abs c < 1e-13 = pure c
  | otherwise = bisection f a c <|> bisection f c b
  where c = (b + a) / 2

findRoot :: Alternative f => (Double -> Double) -> Double -> f Double
findRoot f x0 = go x0 (x0+dx) <|> go x0 (x0-dx)
  where
    go x x' | abs(x') > 1e6 = empty
            | otherwise = bisection f x x' <|> go x' (x'+2*(x'-x))
    dx = 1e-5

main :: IO ()
main = do
  let x = findRoot @ [] (\x -> sinc x - 0.01) 1
  print x
