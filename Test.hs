import Test.QuickCheck
import Data.List
import Debug.Trace

newtype SmallInt = Small Int deriving (Show)
instance Arbitrary SmallInt where
  arbitrary = elements $ map Small [1..7]

groupsOf :: Int -> [a] -> [[a]]
groupsOf size list = groupsOf' list size []

groupsOf' :: [a] -> Int -> [[a]] -> [[a]]
groupsOf' list numGroups result
  | numGroups == 0 = result
  | otherwise = groupsOf' (drop takeAmt list) (numGroups - 1) (result ++ [take takeAmt list])
  where
    takeAmt = (length list) `div` numGroups
{-
hasEveryValue size = 
  let vals = concat $ getList $ hilbertValues size in
    sort vals == [0..(2 ^ size) ^ 2 - 1]
-}

main = do
  --quickCheck ( (\r -> rectsIntersect r r ) :: Rect -> Bool)
  --quickCheck ( (\ (Small s) -> hasEveryValue s) :: SmallInt -> Bool)

  quickCheck ( (\list -> (length (2 `groupsOf` list)) == 2) :: [Int] -> Bool)


