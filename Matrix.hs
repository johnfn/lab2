-- matrix.hs
--
-- Defines a matrix type (really just a 2d list) and some simple operations over it.

module Matrix (
  get2DM,
  getLen,
  getList,
  Matrix,
  rotateMatrix,
  rotateTimes,
  hilbertValues
) where

import Data.List

newtype Matrix a = Matrix [[a]]

--TODO: Remove
get2D :: [[a]] -> Int -> Int -> a
get2D m x y = (m !! y) !! x

get2DM :: Matrix a -> Int -> Int -> a
get2DM (Matrix m) x y = (m !! y) !! x

getLen :: Matrix a -> Int
getLen (Matrix m) = length m

getList :: Matrix a -> [[a]]
getList (Matrix m) = m

instance Functor Matrix where
  fmap fn m = Matrix [[ fn (get2DM m i j) | i <- [0..len]] | j <- [0..len]]
    where len = (getLen m) - 1

instance Show a => Show (Matrix a) where
  show (Matrix a) = intercalate "\n" (fmap show a)

-- Rotates m counter-clockwise
rotateMatrix :: Matrix Int -> Matrix Int
rotateMatrix (Matrix m) = Matrix [[ get2D m (len - y) x | x <- [0..len]] | y <- [0..len]]
  where len = length m - 1

-- Rotates m counter-clockwise n times
rotateTimes :: Matrix Int -> Int -> Matrix Int
rotateTimes m 0 = m
rotateTimes m n = rotateTimes (rotateMatrix m) (n - 1)


hilbertValues :: Int -> Matrix Int
hilbertValues 1 = Matrix [[1, 2], [0, 3]]
hilbertValues size = Matrix [[ get2DM (get2DM components (x `div` oldWidth) (y `div` oldWidth)) (x `mod` oldWidth) (y `mod` oldWidth) | x <- [0..newWidth - 1]] | y <- [0..newWidth - 1]]
  where 
    oldWidth    = 2 ^ (size - 1)
    newWidth    = 2 ^ size --TODO newDimension might be clearer
    smallerSize = (getLen smaller) ^ 2

    smaller = hilbertValues (size - 1)
    smallerFlipped = fmap (\x -> smallerSize - x - 1) smaller
    bottomLeft  =                          (rotateTimes smallerFlipped 3)
    topLeft     = fmap (+ smallerSize * 1)              smaller
    topRight    = fmap (+ smallerSize * 2)              smaller
    bottomRight = fmap (+ smallerSize * 3) (rotateTimes smallerFlipped 1)
    components  = Matrix [[topLeft, topRight], [bottomLeft, bottomRight]]

