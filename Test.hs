import Test.QuickCheck
import HTree hiding (main)
import Rect
import Ptr

ensureChildren :: HTree -> HTree
ensureChildren t@(Node entries _ _) = if (null entries) then (Node [t] (Rect 0 0 5 5) 99) else t

checkCVValidity :: HTree -> Bool
checkCVValidity (Node _ r _) =
  case r of (Rect a b c d) -> (a <= c) && (b <= d)

main = do
    quickCheck( (\r -> checkCVValidity $ (createNodeFrom [r])) :: HTree -> Bool)

    quickCheck( (\r -> let withChildren = ensureChildren r in
                           (length (getChildren (newPtr childLens) withChildren) > 0)) :: HTree -> Bool)

    quickCheck( (\r1 r2 -> (_lhv $ insertRect (insertRect tree r1) r2) ==
                           (_lhv $ insertRect (insertRect tree r2) r1)) :: Rect -> Rect -> Bool)

    quickCheck ( (\r -> (_rect $ createNodeFrom r) == (_rect $ createNodeFrom (reverse r))) :: [HTree] -> Bool)

    quickCheck( (\r -> (_rect $ foldl insertRect tree r) ==
                       (_rect $ foldl insertRect tree (reverse r))) :: [Rect] -> Bool)

  where
    big = foldl insertRect tree (map (\x -> Rect 0 0 1 x) [1..100])
    tree = Node [] (Rect 0 0 0 0) 100
