{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-} 
import Fmt
import Data.Text
import Data.Monoid
import Data.Semigroup (Min(..), Max(..))

class SVG a where
  toSVG :: a -> String

instance SVG (Float,Float) where
  toSVG (x,y) = ""#|x|#","#|y|#""
