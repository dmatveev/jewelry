module Point
       (
         offsetPoint
       ) where

import Basics



instance GameObject Point where
  moveInto _ p = p

  moveTo (Point row col) d = Point (row + dRow) (col + dCol)
    where (dRow, dCol) = case d of
            ToLeft  -> (0, -1)
            ToRight -> (0,  1)
            ToDown  -> (1,  0)

  position = id
  

offsetPoint :: Point -> Int -> Int -> Point
offsetPoint (Point pRow pCol) dRow dCol =
  Point (pRow + dRow) (pCol + dCol)
