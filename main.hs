import Rect
import Data.Maybe
import Data.Ord
import Debug.Trace
import Data.Lens.Common
import Control.Category
import Prelude hiding ((.))
import Data.List
import Rect
import Ptr

data HTree = Node { _entries :: [HTree]
                  , _rect    :: Rect
                  , _lhv     :: Int
                  } deriving (Eq)

-- This is where you start to see Haskell's sex appeal
showHTree indent (Node ch mbr lhv) = 
  (concat $ replicate indent "  ") ++ "lhv: " ++ (show lhv) ++ " rect: " ++ (show mbr) ++ "\n" ++ (concat $ map (showHTree (indent + 2)) ch)

instance Show HTree where
  show n = showHTree 0 n

-- lens for a single child of a Node
childLens n = lens (\t -> (_entries t) !! n) -- get
                   (\child node -> node {_entries = (updateList (_entries node) child n)}) -- set


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
updateList :: [a] -> a -> Int -> [a]
updateList list val pos =
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
addToNode leaf root addition = 
  update leaf root (\(Node oldEntries _ _) -> (deref root leaf) {_entries = oldEntries ++ [addition]})

setNode :: Ptr HTree -> HTree -> [HTree] -> HTree
setNode leaf root newContents = update leaf root (\node -> node {_entries = newContents})

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
    potentialChildren = (filter (\child -> (_lhv child) > hValue) children)
    child = if null potentialChildren
              then maximumBy (comparing _lhv) children
              else minimumBy (comparing _lhv) potentialChildren
    index = fromJust $ findIndex (== child) children

-- Core functionality. Inserts provided rect into provided Hilbert Tree,
-- returning a new tree.
insertRect :: HTree -> Rect -> HTree
insertRect root rect 
  | trace ("insertRect: " ++ show chosenLeaf ++ show rect) False = undefined
  | full $ deref root chosenLeaf =
      let (newNode, newRoot) = handleOverflow chosenLeaf rect root in
        adjustTree chosenLeaf (Just $ getSiblings chosenLeaf root) newNode newRoot
  | otherwise = 
    let newRoot = addToNode chosenLeaf root (Node [] rect (hilbertValue rect)) in
      adjustTree chosenLeaf Nothing Nothing newRoot

  where
    chosenLeaf = chooseLeaf root rect

distributeEntriesToNodes :: [[HTree]] -> [Ptr HTree] -> HTree -> HTree

distributeEntriesToNodes [] [] root = root
distributeEntriesToNodes grps sibs root
  | otherwise = distributeEntriesToNodes (tail grps) (tail sibs) newRoot
  where
    newRoot = setNode (head sibs) root (head grps)

handleOverflow :: Ptr HTree -> Rect -> HTree -> (Maybe HTree, HTree)
handleOverflow node rect root
  | trace ("handleOverflow: " ++ show allFull) False = undefined
  | otherwise = 
    if allFull
      then let updatedRoot = distributeEntriesToNodes (tail entryGroups) (getSiblings node root) root
               (newMBR, newLHV) = calcValues (Node (head entryGroups) undefined undefined)
               splitNode   = Node (head entryGroups) newMBR newLHV in
                 (Just splitNode, updatedRoot)
      else let updatedRoot = distributeEntriesToNodes entryGroups (getSiblings node root) root in
        (Nothing, updatedRoot)
  where
    siblingList = getSiblings node root
    newEntry = Node [] rect (hilbertValue rect)
    entries :: [HTree] = (concat $ (map _entries $ (map (deref root) siblingList))) ++ [newEntry]
    allFull = all full (map (deref root) (getSiblings node root))
    entryGroups :: [[HTree]] = groupsOf ((length siblingList) + (if allFull then 1 else 0)) entries

--TODO - i dont use siblings (var) here.

--createdNode => split
adjustTree :: Ptr HTree -> Maybe [Ptr HTree] -> Maybe HTree -> HTree -> HTree
adjustTree updatedNode siblings createdNode root
  | trace ("adj" ++ (show updatedNode) ++ "\n" ++ (show root)) False = undefined
  | otherwise = 
      if isRoot updatedNode
        then 
          if isJust createdNode --split root! rejoin into one big root.
            then let (newMBR, newLHV) = calcValues (Node [root, fromJust createdNode] undefined undefined) in
                   Node [root, fromJust createdNode] newMBR newLHV
            else updateMbrLhv updatedNode root
        else let (pp, updatedRoot) = propNodeSplit
                 updatedRoot2 = updateMbrLhv updatedNode updatedRoot in
          
          adjustTree (parent updatedNode) (Just $ getSiblings (parent updatedNode) updatedRoot2) pp updatedRoot2

  where
    -- better name might be maybePropNodeSplit...
    propNodeSplit :: (Maybe HTree, HTree)  
    propNodeSplit = 
      let np = parent updatedNode in
        case createdNode of
          Just newNode -> -- this is NN
            if full $ (deref root np)
              then handleOverflow np (_rect newNode) root
              else (Nothing, addToNode np root newNode)
          Nothing ->
            (Nothing, root)

    updateMbrLhv :: Ptr HTree -> HTree -> HTree
    updateMbrLhv node root = 
        update node root (\(Node entries rect lhv) -> (Node entries newMBR newLHV))
      where
        (newMBR, newLHV) = calcValues (deref root node)

main = do
    print $ "Before"
    print $ tree
    print $ "Add 1"
    print $ (insertRect tree (Rect 0 0 2 2))
    print $ "Add 2"
    print $ foldl insertRect tree [Rect 0 0 2 1, Rect 0 0 2 2, Rect 0 0 2 3, Rect 0 0 2 4, Rect 0 0 2 5, Rect 0 0 2 6, Rect 0 0 2 7]
    --print $ insertRect (insertRect (insertRect tree (Rect 0 0 2 2)) (Rect 0 0 5 5)) (Rect 0 0 3 3)
  where
    tree = Node [] (Rect 0 0 0 0) 0
