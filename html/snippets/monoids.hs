{-# LANGUAGE DeriveFoldable #-}
import Data.Monoid
import Data.List
import Data.Ord
import Data.Functor.Contravariant
import Graphics.EasyPlot

pareto' lst = foldl include' [] $ sortBy (comparing fst) lst
  where
    include' [] x = [x]
    include' (y:ys) x | snd x <= snd y = x:y:ys
                      | otherwise = y:ys

pareto lst = map fst $ paretoBy (\(x,y) -> [x,y,x*y]) lst

paretoBy f lst = foldr include [] $ map (\x -> (x, f x)) lst

include x [] = [x]
include x (y:ys) = case dominating (snd x) (snd y) of
  GT -> y:ys
  EQ -> y:include x ys
  LT -> include x ys
  
dominating a b = foldl1 dom $ zipWith compare a b
  where
    a `dom` b = if a == b then a else EQ

pts = [(sin (5*a)*a, a*cos (5*a)) | a <- [0..10000]]

main =  --plot X11 $ Data2D [Title "Sample Data"] [] (pareto pts)
  print $ length (pareto'' pts)

pareto'' lst = map fst $ paretoBy' (\(x,y) -> [x,y,x*y]) lst
mergePareto p1 p2 = foldl' (flip include) p1 p2
paretoBy' f lst = go $ map (\x -> (x, f x)) lst
  where go [] = []
        go [x] = [x]
        go lst = go a `mergePareto` go b
          where (a,b) = splitAt (length lst `div` 2) lst
