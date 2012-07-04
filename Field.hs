module Field
       (
         Field(..)
       , mkField
       , numRows
       , numCols
       , cellAt
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
