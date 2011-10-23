
module Rect (Rect(..), rectsIntersect) where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

data Rect = Rect { _left   :: Int
                 , _top    :: Int
                 , _right  :: Int
                 , _bottom :: Int
                 } deriving (Show, Eq)

instance Arbitrary (Rect) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    if a <= b && c <= d
      then return (Rect a c b d)
      else if a >= b && c <= d
             then return (Rect b c a d)
             else if a <= b && c >= d
                    then return (Rect a d b c)
                    else if a >= b && c >= d
                         then return (Rect b d a c)
                         else error "oh god, the laws of mathematics have been violated"

-- are the coordinates of the rect valid?
validRect :: Rect -> Bool
validRect (Rect left top right bottom) = (left <= right) && (top <= bottom)
  
-- do r1 and r2 intersect?
rectsIntersect :: Rect -> Rect -> Bool
rectsIntersect r1@(Rect r1Left r1Top r1Right r1Bottom) r2@(Rect r2Left r2Top r2Right r2Bottom) =
  if validRect r1 && validRect r2
    then (not (r2Left   > r1Right  || 
               r2Right  < r1Left   || 
               r2Top    > r1Bottom || 
               r2Bottom < r1Top    ))
    else 
      if (not $ validRect r1) 
        then (error $ "Rect coordinates " ++ (show r1) ++ " out of order")
        else (error $ "Rect coordinates " ++ (show r2) ++ " out of order")
