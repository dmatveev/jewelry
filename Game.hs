{-# LANGUAGE TemplateHaskell #-}

module Game where

import Data.List (intersperse)
import Control.Monad (forM_)
import Control.Monad.ST
import Data.Array.ST
import System.Random

import Data.Mutators

import Basics
import Point
import Field
import Figure


data GameState = GameState {
    figure :: Figure
  , field :: Field
  , ticks :: Integer
  }

genMutators ''GameState


mkGame :: (Int, Int) -> Integer -> GameState
mkGame (rs, cs) seed =
  GameState { field = mkField rs cs
            , figure = Figure (Point 1 1) [Yellow, Purple, White]
            , ticks = seed
            }


shuffleFigure :: GameState -> GameState
shuffleFigure gs = modFigure gs $ \f -> shuffle f


throwNewFigure :: GameState -> GameState
throwNewFigure gs =
  setFigure gs $ Figure (Point 1 1) jewels
  where jewels = map (allJewels !!) idxs
        idxs = map (\t -> abs t `mod` length allJewels)
               $ take 3
               $ randoms rndGen
        allJewels = [Red, Green, Blue, Yellow, Purple, White]
        rndGen = mkStdGen $ fromIntegral $ ticks gs


userMoveFigure :: Direction -> GameState -> GameState
userMoveFigure d gs =
  if canMoveFigure d gs
  then moveFigure d gs
  else if d == ToDown && canLandFigure gs
       then throwNewFigure $ landFigureDown gs
       else gs


isInside :: Point -> Field -> Bool
isInside (Point prow pcol) fld =
  (&&) (prow <= numRows fld && prow >= 1)
       (pcol <= numCols fld && pcol >= 1)


canLandFigure :: GameState -> Bool
canLandFigure gs = (&&) (figTopPoint `isInside` fld)
                        ((||) (not (newTailPoint `isInside` fld))
                              (fld `cellAt` newTailPoint /= Empty))
  where figTopPoint = figPos fig
        newTailPoint = last $ figPoints $ (fig `moveTo` ToDown)
        fig = figure gs
        fld = field gs


landFigureDown :: GameState -> GameState
landFigureDown gs = runST $ do
    arr <- (thaw $ fieldMatrix $ field gs)
           :: ST s (STArray s (Int, Int) Cell)

    forM_ jewels $ \(dRow, jewel) -> do
      writeArray arr (figRow + dRow, figCol) $ Jewel jewel
  
    newArr <- freeze arr
    return $ gs { field = Field newArr }
  where (Point figRow figCol) = figPos fig
        jewels = zip [0,1..] (figJewels fig)
        fig = figure gs


moveFigure :: Direction -> GameState -> GameState
moveFigure d gs = modFigure gs $ \f -> f `moveTo` d


canMoveFigure :: Direction -> GameState -> Bool
canMoveFigure d gs = (&&) (newTailPos `isInside` fld)
                          (all (== Empty) targetCells)
  where newTailPos = last newFigPoints
        targetCells = map (cellAt fld) $ dropOffscreen newFigPoints
        newFigPoints = figPoints $ fig `moveTo` d

        dropOffscreen = filter (\x -> x `isInside` fld)
        
        fld = field gs
        fig = figure gs


printedState :: GameState -> String
printedState gs  = concat $ intersperse "\n" $ rowStrings
  where rowStrings = map strfyRow [1..rs]
        strfyRow i = concat $ map (strfyCell i) [1..cs]
        strfyCell i j = let pt = Point i j
                        in if pt `inside` fig
                           then printed $ fig `jewelAt` pt
                           else strfy $ fld `cellAt` pt

        strfy (Jewel c) = printed c
        strfy Empty = "_"

        printed Red    = "r"
        printed Green  = "g"
        printed Blue   = "b"
        printed Yellow = "y"
        printed Purple = "p"
        printed White  = "w"

        rs = numRows fld
        cs = numCols fld
        fld = field gs
        fig = figure gs


instance Show GameState where
  show = printedState


frepeat :: (a -> a) -> a -> Integer -> a
frepeat _ a 0 = a
frepeat f a n = frepeat f (f a) (n - 1)