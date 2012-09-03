{-# LANGUAGE TemplateHaskell #-}

module Game.Jewelry where

import Data.Array.ST
import Data.List (intersperse, groupBy, foldl', nub)
import Data.Mutators
import Data.STRef

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.ST

import System.Random

import Game.Jewelry.Basics
import Game.Jewelry.Point
import Game.Jewelry.Field
import Game.Jewelry.Figure
import Game.Jewelry.HighScore

data GameState = Playing | GameOver
                 deriving (Eq, Show)

data Game = Game {
    figure  :: Figure
  , field   :: Field
  , ticks   :: Integer
  , result  :: GameResult
  , state   :: GameState
  , hiscore :: HighScore
  }

genMutators ''Game
genMutators ''GameResult

mkGame :: (Int, Int) -> Integer -> Game
mkGame (rs, cs) seed =
  Game { field = mkField rs cs
            , figure  = generateNewFigure seed
            , ticks   = seed
            , result  = gameResult
            , state   = Playing
            , hiscore = highscore
            }


ensureState :: Game -> GameState -> (Game -> Game) -> Game
ensureState g st f | st == state g = f g
                   | otherwise     = g


shuffleFigure :: Direction -> Game -> Game
shuffleFigure d game = ensureState game Playing
                       $ \g -> modFigure g (\f -> shuffle f d)


dropFigure :: Game -> Game
dropFigure game = ensureState game Playing $ \g ->
                  if dropPos `isInside` fld
                  then landFigure $ modFigure g (\f -> setFigPos f dropPos)
                  else setState g GameOver
  where dropPos = Point dropRow fCol
        dropRow = emptyLength - figLength fig + 1
        emptyLength = length $ head $ groupBy empties thisColumn
        thisColumn = fieldCol fld fCol

        (Point fRow fCol) = figPos fig
        fig = figure game
        fld = field game

        empties (_, Empty) (_, Empty) = True
        empties _ _ = False


generateNewFigure :: Integer -> Figure
generateNewFigure seed = Figure pos jewels
  where pos = Point (negate $ pred $ length jewels) 3
        jewels = map (allJewels !!) idxs
        idxs = map (\t -> abs t `mod` length allJewels)
               $ take 3
               $ randoms rndGen
        allJewels = [Cherry, Green, Blue, Orange, Purple, Grape]
        rndGen = mkStdGen $ fromIntegral seed


throwNewFigure :: Game -> Game
throwNewFigure game = ensureState game Playing $ \g ->
  flip modResult (\r -> modTotalFigures r succ) $
  setFigure g (generateNewFigure $ ticks game)


moveFigure :: Direction -> Game -> Game
moveFigure d game = ensureState game Playing $ \g ->
  if canMoveFigure d g
  then modFigure g $ \f -> f `moveTo` d
  else if d == ToDown
       then if canLandFigure g
            then landFigure g
            else setState g GameOver
       else g


isInside :: Point -> Field -> Bool
isInside (Point prow pcol) fld =
  (&&) (prow <= numRows fld && prow >= 1)
       (pcol <= numCols fld && pcol >= 1)


canLandFigure :: Game -> Bool
canLandFigure game = (&&) (figTopPoint `isInside` fld)
                          ((||) (not (newTailPoint `isInside` fld))
                                (fld `cellAt` newTailPoint /= Empty))
  where figTopPoint = figPos fig
        newTailPoint = last $ figPoints $ (fig `moveTo` ToDown)
        fig = figure game
        fld = field game


stampFigure :: Game -> Game
stampFigure game = runST $ do
    arr <- (thaw $ fieldMatrix $ field game)
           :: ST s (STArray s (Int, Int) Cell)

    forM_ jewels $ \(dRow, jewel) -> do
      writeArray arr (figRow + dRow, figCol) $ Jewel jewel
  
    newArr <- freeze arr
    return $ game { field = Field newArr }
  where (Point figRow figCol) = figPos fig
        jewels = zip [0,1..] (figJewels fig)
        fig = figure game


landFigure :: Game -> Game
landFigure game = throwNewFigure $ fireCells $ stampFigure game


canMoveFigure :: Direction -> Game -> Bool
canMoveFigure d game = (&&) (newTailPos `isInside` fld)
                            (all (== Empty) targetCells)
  where newTailPos = last newFigPoints
        targetCells = map (cellAt fld) $ dropOffscreen newFigPoints
        newFigPoints = figPoints $ fig `moveTo` d

        dropOffscreen = filter (\x -> x `isInside` fld)
        
        fld = field game
        fig = figure game


sameJewel :: (Point, Cell) -> (Point, Cell) -> Bool
sameJewel (_, (Jewel j1)) (_, (Jewel j2)) = j1 == j2
sameJewel _ _ = False


firingCells :: [(Point, Cell)] -> [(Point, Cell)]
firingCells cs = concat $ filter isFiring $ groupBy sameJewel cs
  where isFiring js = length js >= 3


afford :: [(Point, Cell)] -> Game -> Game
afford cs g = if null cs
              then g
              else modResult g $ \r -> modTotalScore r (+ firedCost)

  where firedCost = baseCost + sum bonuses
        bonuses   = (+ cellCost)  <$>
                    (* bonusCost) <$>
                    take (numCells - 3) [1, 2..]

        numCells  = length cs
        cellCost  = 100
        baseCost  = 3 * cellCost
        bonusCost = 10


dropFiring :: Game -> [(Point, Cell)] -> Game
dropFiring game cs = runST $ do
    arr <- (thaw $ fieldMatrix $ field game)
           :: ST s (STArray s (Int, Int) Cell)

    -- erase the firing cells from the matrix
    forM_ cs $ \(Point r c, _) -> writeArray arr (r, c) $ Empty

    -- move down the hanging jewels in the each column
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
    return $ afford cs $ game { field = Field newArr }
  where (rows, cols) = (numRows fld, numCols fld)
        fld = field game

        -- a tiny FSM (state is the second argument)
        gotEmptyField :: Point -> Maybe Point -> Maybe Point
        gotEmptyField p Nothing  = Just p
        gotEmptyField _ oldState = oldState


fireCells :: Game -> Game
fireCells game = let (cells, game') = fireCellsOnce game
                 in if null cells then game'
                    else fireCells game'


fireCellsOnce :: Game -> ([(Point, Cell)], Game)
fireCellsOnce game = (foundCells, dropFiring game foundCells)
  where foundCells = nub $ firingCells allCells
        allCells = concat
                   $ map (\cs -> concat $ map firingCells cs)
                   $ [allRows, allCols, diagsL, diagsR]
        allRows = fieldRows fld
        allCols = fieldCols fld
        (diagsL, diagsR) = fieldDiags fld
        fld = field game


printedState :: Game -> String
printedState game  = concat $ intersperse "\n" $ rowStrings
  where rowStrings = map strfyRow [1..rs]
        strfyRow i = concat $ map (strfyCell i) [1..cs]
        strfyCell i j = let pt = Point i j
                        in if pt `inside` fig
                           then printed $ fig `jewelAt` pt
                           else strfy $ fld `cellAt` pt

        strfy (Jewel c) = printed c
        strfy Empty = "_"

        printed Cherry = "c"
        printed Green  = "g"
        printed Blue   = "b"
        printed Orange = "o"
        printed Purple = "p"
        printed Grape  = "g"

        rs = numRows fld
        cs = numCols fld
        fld = field game
        fig = figure game
