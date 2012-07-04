module Game where

import Data.List (intersperse)
import Control.Monad (forM_)
import Control.Monad.ST
import Data.Array.ST

import Basics
import Point
import Field
import Figure


data GameState = GameState {
    figure :: Figure
  , field :: Field  
  }


mkGame :: Int -> Int -> Figure -> GameState
mkGame rs cs fig =
  GameState {
    figure = fig
  , field = mkField rs cs
  }


throwNewFigure :: GameState -> GameState
throwNewFigure gs =
  gs { figure = Figure (Point 1 1) [Yellow, Purple, White] }


tick :: GameState -> GameState
tick gs =
  if canMoveFigure ToDown gs
  then moveFigure ToDown gs
  else if canLandFigure gs
       then throwNewFigure $ landFigureDown gs
       else gs


isInside :: Point -> Field -> Bool
isInside (Point prow pcol) fld =
  (&&) (prow <= numRows fld && prow >= 1)
       (pcol <= numCols fld && pcol >= 1)


canLandFigure :: GameState -> Bool
canLandFigure gs = figTopPoint `isInside` fld
  where figTopPoint = figPos $ figure gs
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
moveFigure d gs = gs { figure = movedFig }
  where movedFig = (figure gs) `moveTo` d


canMoveFigure :: Direction -> GameState -> Bool
canMoveFigure d gs = (&&) (newTailPos `isInside` fld)
                          (any (== Empty) targetCells)
  where newTailPos = last newFigPoints
        targetCells = map (cellAt fld) $ dropOffscreen newFigPoints
        newFigPoints = figPoints $ fig `moveTo` d

        dropOffscreen = filter (\x -> x `isInside` fld)
        
        fld = field gs
        fig = figure gs


canMoveFigureDown :: GameState -> Bool
canMoveFigureDown gs = nextPoint `isInside` fld && nextCell == Empty
  where nextCell = (field gs) `cellAt` nextPoint
        nextPoint = offsetPoint (figPos fig) (figLength fig) 0

        fig = figure gs
        fld = field gs


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


frepeat :: (a -> a) -> a -> Int -> a
frepeat _ a 0 = a
frepeat f a n = frepeat f (f a) (n - 1)