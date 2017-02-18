module Triscape (Triangle(..),landScape) where

import Data.Hashable
import qualified System.Random as R

type Pt = (Float,Float,Float)

data Triangle = Triangle Pt Pt Pt deriving Show

landScape :: Int -> [Triangle] -> Int -> [Triangle]
landScape n t s = iterate (>>= split s) t !! n

split :: Int -> Triangle -> [Triangle]
split s (Triangle a b c) = [ Triangle a ab ac
                           , Triangle ab b bc
                           , Triangle bc c ac
                           , Triangle ab bc ac ]
  where
    ab = mid a b
    ac = mid a c
    bc = mid b c

    mid p1@(ax,ay,az) p2@(bx,by,bz)
      = ((ax+bx)/2,(ay+by)/2,(az+bz)/2 + dz)
      where dz = 0.3*dist p1 p2*rnd s p1 p2

dist :: Pt -> Pt -> Float
dist (ax,ay,az) (bx,by,bz)
  = sqrt $ (bx-ax)**2+(by-ay)**2+(bz-az)**2
      
rnd :: Int -> Pt -> Pt -> Float
rnd s p1 p2
  | p1 > p2 = rnd s p2 p1
  | otherwise = fst $ R.randomR (-1,1) g
  where g = R.mkStdGen (hashWithSalt s (p1,p2))
