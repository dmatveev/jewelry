module Figure
       (
         Figure(..)

       , figLength
       , figPoints

       , inside
       , jewelAt    
       ) where

import Basics
import Point


data Figure = Figure {
    figPos :: Point
  , figJewels :: [Jewel]
  } deriving (Eq, Show)


instance GameObject Figure where
  moveInto f p = f { figPos = p }

  moveTo f d = f { figPos = (figPos f) `moveTo` d }

  position = figPos
  

figLength :: Figure -> Int
figLength = length . figJewels


figPoints :: Figure -> [Point]
figPoints f = map createPoint $ take (figLength f) [0,1..]
  where createPoint i = offsetPoint (figPos f) i 0


inside :: Point -> Figure -> Bool
inside (Point prow pcol) fig =
    (pcol == fcol) && (prow >= frow) && (prow < (frow + flen))
  where (Point frow fcol) = figPos fig
        flen = figLength fig


jewelAt :: Figure -> Point -> Jewel
jewelAt fig pt@(Point prow _) = (figJewels fig) !! (prow - frow)
  where (Point frow _) = figPos fig
