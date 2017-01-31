import Plot

dot :: Point -> Point -> Double
dot (x1,y1) (x2,y2) = x1*x2 + y1*y2

norm :: Point -> Double
norm v = sqrt $ dot v v 

minus :: Point -> Point -> Point
minus (x1,y1) (x2,y2) = (x2 - x1, y2 + y1)

mult :: Double -> Point -> Point 
mult s (x,y) = (s*x, s*y)

cosine :: Point -> Point -> Point -> Double
cosine p1 p2 p3 = dot v1 v2 / (norm v1 * norm v2)
  where v1 = p2 `minus` p1
        v2 = p3 `minus` p2

triples :: [a] -> [(Maybe a, Maybe a, Maybe a)]
triples tbl = zip3 t1 t2 t3
  where
    t1 = Nothing : init t2
    t2 = Just <$> tbl
    t3 = tail t2 ++ [Nothing]

adjust :: (Maybe Point, Maybe Point, Maybe Point) -> [Point]
adjust (Just p1, Just p2, Just p3)
  | a > maxCos = []
  | a > minCos = [p2]
  | otherwise = [p1',p2,p3']
  where
    a = cosine p1 p2 p3
    maxCos = cos $ pi/180
    minCos = cos $ 5*pi/180
    p1' = p2 `minus` ((1/3) `mult` (p1 `minus` p2))
    p3' = p2 `minus` ((1/3) `mult` (p2 `minus` p3))
adjust (_, Just p2, _) = [p2]

main :: IO ()
main = return ()
