{-# OPTIONS -Wall #-}

import Data.List
import Data.Bits
import Data.List.Split
import Data.Maybe
import Data.Ord
import Data.Lens.Common
import Test.QuickCheck hiding ((.&.))
import Control.Monad
import Control.Category
import Prelude hiding ((.))
import System.Environment

import Rect
import Ptr
import Matrix

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
    maxDepth <- choose (0, 8)
    t <- arbitraryTree maxDepth (Rect 0 0 largestValue largestValue) largestValue
    return t

-- This is where you start to see Haskell's sex appeal
showHTree indent (Node ch mbr lhv) =
  (concat $ replicate indent "  ") ++ "lhv: " ++ (show lhv) ++ " rect: " ++ (show mbr) ++ "\n" ++ (concat $ map (showHTree (indent + 2)) ch)

instance Show HTree where
  show n = showHTree 0 n

-- lens for a single child of a Node
childLens n 
  | otherwise = 
    lens (\t -> (_entries t) !! n) -- get
         (\child node -> node {_entries = (updateList (_entries node) child n)}) -- set


--TODO - there should be an idiom for this stuff, figure out if time (there wont be time)
createNodeFrom :: [HTree] -> HTree
createNodeFrom [] = (Node [] (Rect 0 0 0 0) 0)
createNodeFrom entries = Node entries newMBR newLHV
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


hilbertValue :: Rect -> Int 
hilbertValue (Rect left right top bottom) = 
    hilbertValue' n ((left + right) `div` 2) ((top + bottom) `div` 2) (n `div` 2) 0
  where
    n = 16

-- Written with help from http://en.wikipedia.org/wiki/Hilbert_curve
hilbertValue' n x y s d =
  if s == 0 then d else hilbertValue' n x y (s `div` 2) (s * s * ((3 * rx) `xor` ry))
  where
    rx = if (x .&. s) > 0 then 1 else 0
    ry = if (y .&. s) > 0 then 1 else 0

--add rect to leaf, returning new tree
addToNode :: Ptr HTree -> HTree -> HTree -> HTree
addToNode leaf root addition 
  | otherwise = 
      update leaf root (\(Node oldEntries _ _) -> (deref root leaf) {_entries = oldEntries ++ [addition]})

setNode :: Ptr HTree -> HTree -> [HTree] -> HTree
setNode leaf root newContents 
  | otherwise = update leaf root (\node -> createNodeFrom newContents)

search :: Rect -> HTree -> [Rect]
search val n@(Node children rect _)
  | isLeaf n = filter (rectsIntersect val) (map _rect children)
  | otherwise = concat (map (search val) children)

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

handleOverflow :: Ptr HTree -> HTree -> HTree -> (Maybe HTree, HTree)
handleOverflow node newEntry root
  | allFull =
      let updatedRoot      = updateNodes (tail entryGroups) siblingList root
          splitNode        = createNodeFrom (head entryGroups) in
            (Just splitNode, updatedRoot)
  | otherwise = (Nothing, updateNodes entryGroups siblingList root)
  where
    siblingList = getSiblings node root
    entries = [newEntry] ++ (concat (map _entries (map (deref root) siblingList))) 
    allFull = all full (map (deref root) siblingList)
    numGroups = (length siblingList) + (if allFull then 1 else 0)
    entryGroups = numGroups `groupsOf` entries


adjustTree :: Ptr HTree -> Maybe HTree -> HTree -> HTree
adjustTree updatedNode createdNode root
    | isRoot updatedNode =
        case createdNode of
          Just newNode -> createNodeFrom [updateMbrLhv updatedNode root, newNode]
          Nothing -> updateMbrLhv updatedNode root
    | otherwise = 
        let updatedRoot = updateMbrLhv updatedNode root
            (pp, updatedRoot2) = propNodeSplit updatedRoot in
          adjustTree (parent updatedNode) pp updatedRoot2

  where
    -- better name might be maybePropNodeSplit...
    propNodeSplit :: HTree -> (Maybe HTree, HTree)  
    propNodeSplit uroot = 
      let np = parent updatedNode in
        case createdNode of
          Just newNode -> -- this is NN
            if full $ (deref uroot np)
              then handleOverflow np newNode uroot
              else (Nothing, addToNode np uroot newNode)
          Nothing -> (Nothing, uroot)

    updateMbrLhv :: Ptr HTree -> HTree -> HTree
    updateMbrLhv node uroot =
        update node uroot (\(Node entries rect lhv) -> createNodeFrom entries)

ensureChildren :: HTree -> HTree
ensureChildren t@(Node entries _ _) = if (null entries) then (Node [t] (Rect 0 0 5 5) 99) else t

checkCVValidity :: (Rect, Int) -> Bool
checkCVValidity r =
  case r of ((Rect a b c d), _) -> (a <= c) && (b <= d)

getFileOrFail :: IO String
getFileOrFail = do
  args <- getArgs
  if length args == 0
    then return (error "Was not provided with a file name to read. Exiting.")
    else return (head args)

toRect :: [Int] -> Rect
toRect c = 
  let xs = [c !! 0, c !! 2, c !! 4, c !! 6]
      ys = [c !! 1, c !! 3, c !! 5, c !! 7] in
    Rect (minimum xs) (minimum ys) (maximum xs) (maximum ys)
    
toRects :: [String] -> [Rect]
toRects contents =
  map (\str -> (toRect ((map read (splitOn "," str)) :: [Int]))) contents

countEntries :: HTree -> Int
countEntries t
  | isLeaf t = length (_entries t)
  | otherwise = foldl1 (+) (map countEntries (_entries t))


processInput :: HTree -> IO ()
processInput hTree = do
  line <- getLine
  putStrLn $ show (search (toRect ((map read (splitOn "," line)) :: [Int])) hTree )

  processInput hTree

main = do
    fileName <- getFileOrFail
    contents <- fmap lines $ readFile fileName
    let rects = toRects contents

    let hTree = foldl insertRect tree rects

    --quickCheck( (\r -> checkCVValidity $ (calcValues (ensureChildren r))) :: HTree -> Bool)
    --quickCheck( (\r -> let withChildren = ensureChildren r in
                         --(length (getChildren (newPtr childLens) withChildren) > 0)) :: HTree -> Bool)
    --print $ hTree

    processInput hTree

    --print $ (search (toRect [3458, 2482, 3458, 2456, 3570, 2456, 3570, 2482]) hTree)

    {-
    quickCheck( (\r1 r2 -> (_lhv $ insertRect (insertRect tree r1) r2) ==
                           (_lhv $ insertRect (insertRect tree r2) r1)) :: Rect -> Rect -> Bool)
    -}

    --quickCheck ( (\r -> (_rect $ createNodeFrom r) == (_rect $ createNodeFrom (reverse r))) :: [HTree] -> Bool)

    {-
    quickCheck( (\r -> (_rect $ foldl insertRect tree r) ==
                       (_rect $ foldl insertRect tree (reverse r))) :: [Rect] -> Bool)
                       -}

    --getChildren :: Ptr HTree -> HTree -> [Ptr HTree]
    --print $ insertRect (insertRect (insertRect tree (Rect 0 0 2 2)) (Rect 0 0 5 5)) (Rect 0 0 3 3)
  where
    tree = Node [] (Rect 0 0 0 0) 100
    big = foldl insertRect tree (map (\x -> Rect 0 0 1 x) [1..1000])

