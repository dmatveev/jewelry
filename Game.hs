{-# LANGUAGE TemplateHaskell #-}

module Game where

import Data.List (intersperse, groupBy, foldl', nub)
import Control.Monad (forM_)
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
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
            , figure = generateNewFigure seed
            , ticks = seed
            }


shuffleFigure :: GameState -> GameState
shuffleFigure gs = modFigure gs $ \f -> shuffle f


generateNewFigure :: Integer -> Figure
generateNewFigure seed = Figure (Point 1 1) jewels
  where jewels = map (allJewels !!) idxs
        idxs = map (\t -> abs t `mod` length allJewels)
               $ take 3
               $ randoms rndGen
        allJewels = [Red, Green, Blue, Yellow, Purple, White]
        rndGen = mkStdGen $ fromIntegral seed


throwNewFigure :: GameState -> GameState
throwNewFigure gs = setFigure gs $ generateNewFigure $ ticks gs


moveFigure :: Direction -> GameState -> GameState
moveFigure d gs =
  if canMoveFigure d gs
  then modFigure gs $ \f -> f `moveTo` d
  else if d == ToDown && canLandFigure gs
       then throwNewFigure $ fireCells $ landFigureDown gs
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


canMoveFigure :: Direction -> GameState -> Bool
canMoveFigure d gs = (&&) (newTailPos `isInside` fld)
                          (all (== Empty) targetCells)
  where newTailPos = last newFigPoints
        targetCells = map (cellAt fld) $ dropOffscreen newFigPoints
        newFigPoints = figPoints $ fig `moveTo` d

        dropOffscreen = filter (\x -> x `isInside` fld)
        
        fld = field gs
        fig = figure gs


firingCells :: [(Point, Cell)] -> [(Point, Cell)]
firingCells cs = concat $ filter isFiring $ groupBy sameJewel cs
  where sameJewel (_, (Jewel j1)) (_, (Jewel j2)) = j1 == j2
        sameJewel _ _ = False
        isFiring js = length js >= 3


dropFiring :: GameState -> [(Point, Cell)] -> GameState
dropFiring gs cs = runST $ do
    arr <- (thaw $ fieldMatrix $ field gs)
           :: ST s (STArray s (Int, Int) Cell)

    -- erase the firing cells from the matrix
    forM_ cs $ \(Point r c, _) -> writeArray arr (r, c) $ Empty

    -- move down hanging jewels in the each column
    forM_ [1 .. cols] $ \col -> do
      state <- newSTRef (Nothing :: Maybe Point)
      forM_ [rows, pred rows .. 1] $ \row -> do
        cell <- readArray arr (row, col)
        case cell of
          Empty -> modifySTRef state (gotEmptyField $ Point row col)
          j@(Jewel _) -> do
            lastEmptyPoint <- readSTRef state
            case lastEmptyPoint of
              Nothing  -> return ()
              Just (Point emRow emCol) -> do
                writeArray arr (emRow, emCol) j
                writeArray arr (row, col) Empty
                writeSTRef state $ Just $ Point (emRow - 1) emCol

    newArr <- freeze arr
    return $ gs { field = Field newArr }
  where (rows, cols) = (numRows fld, numCols fld)
        fld = field gs

        -- a tiny FSM (state is the second argument)
        gotEmptyField :: Point -> Maybe Point -> Maybe Point
        gotEmptyField p Nothing  = Just p
        gotEmptyField _ oldState = oldState


fireCells :: GameState -> GameState
fireCells gs = dropFiring gs $ nub $ firingCells allCells
  where allCells = concat
                   $ map (\cs -> concat $ map firingCells cs)
                   $ [allRows, allCols, diagsL, diagsR]
        allRows = fieldRows fld
        allCols = fieldCols fld
        (diagsL, diagsR) = fieldDiags fld
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
