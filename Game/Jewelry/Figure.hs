{-# LANGUAGE TemplateHaskell #-}

module Game.Jewelry.Figure
       (
         Figure(..)

       , figLength
       , figPoints

       , inside
       , jewelAt

       , shuffle

       , setFigPos
       ) where

import Data.Mutators

import Game.Jewelry.Basics
import Game.Jewelry.Point


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


shuffle :: Figure -> Direction -> Figure
shuffle f d = modFigJewels f (jcycle d)
  where jcycle ToDown    js  = last js : init js
        jcycle ToUp   (j:js) = js ++ [j]
        jcycle _         js  = js
