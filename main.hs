{-# OPTIONS -Wall #-}

import Rect
import Data.Maybe
import Data.Ord
import Debug.Trace
import Data.Lens.Common
import Test.QuickCheck
import Control.Monad
import Control.Category
import Prelude hiding ((.))
import Data.List
import Rect
import Ptr

{-
updateMbrLhv :: Ptr HTree -> HTree -> HTree
updateMbrLhv node uroot 
  | otherwise = 
      update node uroot (\(Node entries rect lhv) -> (Node entries newMBR newLHV))
  where
    (newMBR, newLHV) = calcValues (deref uroot node)
-}

data HTree = Node { _entries :: [HTree]
                  , _rect    :: Rect
                  , _lhv     :: Int
                  } deriving (Eq)

arbitraryTree :: Int -> Rect -> Int -> Gen HTree
arbitraryTree maxDepth (Rect v1 v2 v3 v4) biggestVal = do
    rect <- do
      n1 <- choose (0, v1)
      n2 <- choose (0, v2)
      n3 <- choose (n1, v3)
      n4 <- choose (n2, v4)
      return (Rect n1 n2 n3 n4)

    lhv <- choose (0, biggestVal)

    if maxDepth == 0
      then return (Node [] rect lhv)
      else do 
        t <- oneof [ liftM3 Node (resize 3 $ listOf $ arbitraryTree depth' rect lhv) arbitrary arbitrary]
        return t
  where
    depth' = maxDepth - 1

largestValue :: Int
largestValue = 10000

instance Arbitrary HTree where
  arbitrary = do
    maxDepth <- choose (1, 8)
    t <- arbitraryTree maxDepth (Rect 0 0 largestValue largestValue) largestValue
    return t
-- This is where you start to see Haskell's sex appeal
showHTree indent (Node ch mbr lhv) =
  (concat $ replicate indent "  ") ++ "lhv: " ++ (show lhv) ++ " rect: " ++ (show mbr) ++ "\n" ++ (concat $ map (showHTree (indent + 2)) ch)

instance Show HTree where
  show n = showHTree 0 n

at list val = 
  if val >= (length list)  || val < 0
    then error $ ("AT FAIL AT " ++ show list ++ show val)
    else list !! val

-- lens for a single child of a Node
childLens n 
  | otherwise = 
    lens (\t -> (_entries t) `at` n) -- get
                     (\child node -> node {_entries = (updateList (_entries node) child n)}) -- set


--TODO: Better is createNodeFrom that takes a list of HTree children.

--TODO - there should be an idiom for this stuff, figure out if time (there wont be time)
calcValues :: HTree -> (Rect, Int)
calcValues (Node entries rect lhv) = (newMBR, newLHV)
  where
    newMBR = Rect ((_left   . _rect) (minimumBy (comparing (_left   . _rect)) entries)) 
                  ((_top    . _rect) (minimumBy (comparing (_top    . _rect)) entries))
                  ((_right  . _rect) (maximumBy (comparing (_right  . _rect)) entries))
                  ((_bottom . _rect) (maximumBy (comparing (_bottom . _rect)) entries))
    newLHV = _lhv $ maximumBy (comparing _lhv) entries

-- get all children of a Ptr as [Ptr]s. This breaks the abtraction - in an
-- actual Ptr library it might be better to hide Ptr internals and pass in 
-- a function to do this.
getChildren :: Ptr HTree -> HTree -> [Ptr HTree]
getChildren p@(Ptr list lens) root =
    map (\num -> Ptr (list ++ [num]) lens) [0..numChildren - 1]
  where 
    numChildren = length (_entries $ deref root p)

--incl provided sibling
getSiblings :: Ptr HTree -> HTree -> [Ptr HTree]
getSiblings ptr root
  -- | not $ isRoot root = error "Root must actually be root"
  | isRoot ptr = [ptr] -- need to special case since you can't get parent of root
  | otherwise = getChildren (parent ptr) root

full :: HTree -> Bool
full (Node entries _ _) = (length entries) >= 3

-- Updates an index to new a new value in a list.
updateList :: Show a => [a] -> a -> Int -> [a]
updateList list val pos 
  | otherwise = 
      first ++ [val] ++ rest
  where
    (first, oldVal:rest) = splitAt pos list

-- separate LIST into sublists of size SIZE.
groupsOf :: Show a => Int -> [a] -> [[a]]
groupsOf size list = groupsOf' list size []

groupsOf' ::  Show a =>[a] -> Int -> [[a]] -> [[a]]
groupsOf' list numGroups result
  | numGroups == 0 = result
  | otherwise = groupsOf' (drop takeAmt list) (numGroups - 1) (result ++ [take takeAmt list])
  where
    takeAmt = (length list) `div` numGroups

--TODO
hilbertValue :: Rect -> Int
hilbertValue r = 5

--add rect to leaf, returning new tree
addToNode :: Ptr HTree -> HTree -> HTree -> HTree
addToNode leaf root addition 
  | otherwise = 
      update leaf root (\(Node oldEntries _ _) -> (deref root leaf) {_entries = oldEntries ++ [addition]})

setNode :: Ptr HTree -> HTree -> [HTree] -> HTree
setNode leaf root newContents 
  | otherwise = update leaf root (\node -> node {_entries = newContents})

{-
search :: HTree -> [Rect]
--TODO - no more leaf/node
search (Leaf entities) rect = filter (rectIntersect rect) entities
search (Node leaves) rect = concat (map search leaves)
-}

-- It's a leaf if the only children it has are empty but for their Rect.

isLeaf :: HTree -> Bool
isLeaf (Node children _ _) =
  not $ any (\child -> (length (_entries child)) > 0) children

chooseLeaf :: HTree -> Rect -> Ptr HTree
chooseLeaf root rect = chooseLeaf' root (hilbertValue rect) (newPtr childLens)

chooseLeaf' n@(Node children _ _) hValue ptr
  | isLeaf n = ptr
  | otherwise = chooseLeaf' child hValue (getChild index ptr)
  where
    potentialChildren = (filter (\ch -> (_lhv ch) > hValue) children)
    child = if null potentialChildren
              then maximumBy (comparing _lhv) children
              else minimumBy (comparing _lhv) potentialChildren
    index = fromJust $ findIndex (== child) children

-- Core functionality. Inserts provided rect into provided Hilbert Tree,
-- returning a new tree.
insertRect :: HTree -> Rect -> HTree
insertRect root rect 
  | full $ deref root chosenLeaf =
      let (splitNode, newRoot) = handleOverflow chosenLeaf newNode root in
        adjustTree chosenLeaf splitNode newRoot
  | otherwise = 
    let newRoot = addToNode chosenLeaf root (Node [] rect (hilbertValue rect)) in
      adjustTree chosenLeaf Nothing newRoot

  where
    chosenLeaf = chooseLeaf root rect
    newNode = Node [] rect (hilbertValue rect)

updateNodes :: [[HTree]] -> [Ptr HTree] -> HTree -> HTree
updateNodes [] [] root = root
updateNodes (grp:grps) (sib:sibs) root = updateNodes grps sibs (setNode sib root grp)

--todo rect->node
handleOverflow :: Ptr HTree -> HTree -> HTree -> (Maybe HTree, HTree)
handleOverflow node newEntry root
  | allFull =
      let updatedRoot      = updateNodes (tail entryGroups) siblingList root
          splitNode        = Node (head entryGroups) (Rect 0 0 1 1) 1 in
            (Just splitNode, updatedRoot)
  | otherwise = (Nothing, updateNodes entryGroups siblingList root)
  where
    --TODO ERROR/?????????????????
    siblingList = getSiblings node root
    entries :: [HTree] = [newEntry] ++ (concat (map _entries (map (deref root) siblingList))) 
    allFull = all full (map (deref root) siblingList)
    numGroups = (length siblingList) + (if allFull then 1 else 0)
    entryGroups :: [[HTree]] = numGroups `groupsOf` entries



--createdNode => split
adjustTree :: Ptr HTree -> Maybe HTree -> HTree -> HTree
adjustTree updatedNode createdNode root
    | isRoot updatedNode =
        case createdNode of
          Just newNode -> Node [root, newNode] (Rect 0 0 1 1) 99
            --let (newMBR, newLHV) = calcValues (Node [root, fromJust createdNode] undefined undefined) in
          Nothing -> root --updateMbrLhv updatedNode root
    | otherwise = 
        let (pp, updatedRoot) = propNodeSplit in
                 -- updatedRoot2 = updateMbrLhv updatedNode updatedRoot in
                 -- TODO error here is that im not updating stuff
          adjustTree (parent updatedNode) pp updatedRoot--2

  where
    -- better name might be maybePropNodeSplit...
    propNodeSplit :: (Maybe HTree, HTree)  
    propNodeSplit = 
      let np = parent updatedNode in
        case createdNode of
          Just newNode -> -- this is NN
            if full $ (deref root np)
              then handleOverflow np newNode root
              else (Nothing, addToNode np root newNode)
          Nothing -> (Nothing, root)

ensureChildren :: HTree -> HTree
ensureChildren t@(Node entries _ _) = if (null entries) then (Node [t] (Rect 0 0 5 5) 99) else t

checkCVValidity :: (Rect, Int) -> Bool
checkCVValidity r =
  case r of ((Rect a b c d), _) -> (a <= c) && (b <= d)

main = do
    --quickCheck( (\r -> checkCVValidity $ (calcValues (ensureChildren r))) :: HTree -> Bool)
    --quickCheck( (\r -> let withChildren = ensureChildren r in
                         --(length (getChildren (newPtr childLens) withChildren) > 0)) :: HTree -> Bool)

    quickCheck( (\r1 r2 -> (insertRect (insertRect tree r1) r2) ==
                           (insertRect (insertRect tree r2) r1)) :: Rect -> Rect -> Bool)


    --getChildren :: Ptr HTree -> HTree -> [Ptr HTree]
    print $ foldl insertRect tree (map (Rect 0 0 2) [1..100])
    --print $ insertRect (insertRect (insertRect tree (Rect 0 0 2 2)) (Rect 0 0 5 5)) (Rect 0 0 3 3)
  where
    tree = Node [] (Rect 0 0 0 0) 0
