module Field
       (
         Field(..)
       , mkField
       , numRows
       , numCols
       , cellAt

       , fieldRows
       , fieldCols
       , fieldDiags
       ) where

import Data.Array
import Basics

newtype Field = Field { fieldMatrix :: (Array (Int, Int) Cell) }
              deriving (Show)

mkField :: Int -> Int -> Field
mkField rs cs =
  Field $ array ((1,1), (rs, cs))
                [((i, j), Empty) | i <- [1..rs], j <- [1..cs]]

numRows :: Field -> Int
numRows fld = r
  where (_, (r, _)) = bounds $ fieldMatrix fld

numCols :: Field -> Int
numCols fld = c
  where (_, (_, c)) = bounds $ fieldMatrix fld

cellAt :: Field -> Point -> Cell
cellAt fld (Point r c) = (fieldMatrix fld)!(r, c)


-- Given a rectangle represented by a number of rows and cols,
-- generate all its diagonals
diags :: Int -> Int -> [[(Int, Int)]]
diags rows cols = map diagPoints diagStarts
  where diagPoints pos = take (diagLen pos) $ evolve diagTrans pos

        diagTrans (r, c) = (r - 1, c + 1)

        diagLen (r, c)
          | r < maxLen              = r
          | r >= maxLen && r < rows = maxLen
          | r == rows               = if c <= cols - maxLen + 1
                                      then maxLen
                                      else cols - c + 1

        diagStarts = rDiags ++ cDiags
        rDiags = map (\r -> (r,    1)) [1..rows]
        cDiags = map (\c -> (rows, c)) [2..cols]

        maxLen = min rows cols


-- Mirror each diagonal in the given set for the given rectangle
mirror :: [[(Int, Int)]] -> Int -> Int -> [[(Int, Int)]]
mirror diags _ cols = map mirrorDiag diags
  where mirrorDiag diag = map mirrorCell diag
        mirrorCell (r, c) = (r, cols - c + 1)


cellAt' :: Field -> Int -> Int -> (Point, Cell)
cellAt' fld r c = (p, fld `cellAt` p) where p = Point r c

fieldRow :: Field -> Int -> [(Point, Cell)]
fieldRow fld r = map (cellAt' fld r) [1..numCols fld]

fieldRows :: Field -> [[(Point, Cell)]]
fieldRows fld = map (fieldRow fld) [1..numRows fld]

fieldCol :: Field -> Int -> [(Point, Cell)]
fieldCol fld c = map (\r -> cellAt' fld r c) [1..numRows fld]

fieldCols :: Field -> [[(Point, Cell)]]
fieldCols fld = map (fieldCol fld) [1..numCols fld]

fieldDiags :: Field -> ([[(Point, Cell)]], [[(Point, Cell)]])
fieldDiags fld = (botLeftToTopRight, botRightToTopLeft)
  where botLeftToTopRight = map xtract blDiags
        botRightToTopLeft = map xtract $ mirror blDiags rows cols
        blDiags = diags rows cols
        xtract diag = map (\(r, c) -> cellAt' fld r c) diag
        (rows, cols) = (numRows fld, numCols fld)
