module Plot where

type NumFunc = Double -> Double
type Point = (Double, Double)

tab :: NumFunc -> Double -> Point
tab f x = (x, f x)

type Range = (Double,Double)
type Table = [Point]

tabulate :: NumFunc -> Range -> Int -> Table
tabulate f (x1,x2) n = tab f <$> [x1,x1+h..x2]
  where h = (x2-x1) / fromIntegral n

scale :: Range -> Range -> Double -> Double
scale (a,b) (c,d) x = (d-c)/(b-a)*(x-a)+c

polyline :: Table -> String
polyline pts = concat [ "<polyline points='"
                      , points
                      , "' fill='none' stroke='blue'/>" ]
   where
     points = unwords $ showPoint <$> pts
     showPoint (x,y) = show x ++ "," ++ show y

plot :: NumFunc -> (Range, Range) -> Int -> String
plot f (xRange, yRange) n = undefined
