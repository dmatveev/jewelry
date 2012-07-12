{-# LANGUAGE TemplateHaskell #-}

module Figure
       (
         Figure(..)

       , figLength
       , figPoints

       , inside
       , jewelAt

       , shuffle
       ) where

import Data.Mutators

import Basics
import Point


data Figure = Figure {
    figPos :: Point
  , figJewels :: [Jewel]
  } deriving (Eq, Show)


genMutators ''Figure


instance GameObject Figure where
  moveInto   = setFigPos
  moveTo f d = modFigPos f $ \p -> p `moveTo` d
  position   = figPos
  

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


shuffle :: Figure -> Figure
shuffle f = modFigJewels f $ \(js) -> last js : init js