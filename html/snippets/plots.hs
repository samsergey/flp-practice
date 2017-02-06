import Plot
import Data.Monoid
import Data.Semigroup
import Data.Foldable

data SVG = SVG { range :: (Range, Range)
               , contents :: SVGObject }

type Scaler = Double -> Double 

type SVGObject = (Scaler, Scaler) -> String

--svg (w,h) (xRange,yRange) p = p scale (xRange, 
