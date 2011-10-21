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
  | not $ isRoot ptr = error "Root must actually be root"
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

insertRect :: HTree -> Rect -> HTree
insertRect root rect 
  | full $ deref root chosenLeaf =
      let (newNode, newRoot) = handleOverflow chosenLeaf rect root in
        adjustTree chosenLeaf (Just $ getSiblings chosenLeaf root) newNode newRoot
  | otherwise = addToNode chosenLeaf root (Node [] rect (hilbertValue rect))
  where
    chosenLeaf = chooseLeaf root rect

distributeEntriesToNodes :: [[HTree]] -> [Ptr HTree] -> HTree -> HTree

distributeEntriesToNodes [] [] root = root
distributeEntriesToNodes grps sibs root
  | otherwise = distributeEntriesToNodes (tail grps) (tail sibs) newRoot
  where
    newRoot = setNode (head sibs) root (head grps)

handleOverflow :: Ptr HTree -> Rect -> HTree -> (Maybe HTree, HTree)
handleOverflow node rect root =
    if allFull
      then let updatedRoot = distributeEntriesToNodes (tail entryGroups) (getSiblings node root) root
               --TODO actual bounding thing for group
               splitNode   = Node (head entryGroups) (Rect 9 9 9 9) (hilbertValue rect) in
                 (Just splitNode, updatedRoot)
      else let updatedRoot = distributeEntriesToNodes entryGroups (getSiblings node root) root in
        (Nothing, updatedRoot)
  where
    siblingList = (getSiblings node root)
    newEntry = Node [] rect (hilbertValue rect)
    entries :: [HTree] = concat $ (map _entries $ (map (deref root) siblingList))
    allFull = all full (map (deref root) (getSiblings node root))
    entryGroups :: [[HTree]] = groupsOf ((length siblingList) + (if allFull then 1 else 0)) entries

--TODO - i dont use siblings (var) here.

--createdNode => split
adjustTree :: Ptr HTree -> Maybe [Ptr HTree] -> Maybe HTree -> HTree -> HTree
adjustTree updatedNode siblings createdNode root
  | trace ("??" ++ show updatedNode) False = undefined
  | otherwise = 
      if isRoot updatedNode
          then 
            if isJust createdNode --split root! rejoin into one big root.
              --todo-real values
              then Node [root, fromJust createdNode] (Rect 0 0 100 100) 55
              else root
          else let (pp, updatedRoot) = propNodeSplit
                   updatedRoot2 = updateMbrLhv updatedNode root in
            
            adjustTree (parent updatedNode) (Just $ getSiblings (parent updatedNode) updatedRoot2) pp updatedRoot2

  where
    -- better name might be maybePropNodeSplit...
    propNodeSplit :: (Maybe HTree, HTree)  
    propNodeSplit = 
      let np = parent updatedNode in
        case createdNode of
          Just newNode ->
            if full $ newNode
              then handleOverflow np (_rect newNode) root
              else (Nothing, addToNode np root newNode)
          Nothing ->
            (Nothing, root)

    updateMbrLhv :: Ptr HTree -> HTree -> HTree
    updateMbrLhv node root = 
        update node root (\(Node entries rect lhv) -> (Node entries newMBR newLHV))
      where
        children = map (deref root) (getChildren node root)
        --TODO - there should be an idiom for this stuff, figure out if time (there wont be time)
        newMBR = Rect ((_left   . _rect) (minimumBy (comparing (_left   . _rect)) children)) 
                      ((_top    . _rect) (minimumBy (comparing (_top    . _rect)) children))
                      ((_right  . _rect) (maximumBy (comparing (_right  . _rect)) children))
                      ((_bottom . _rect) (maximumBy (comparing (_bottom . _rect)) children))
        newLHV = _lhv $ maximumBy (comparing _lhv) children


main = do
    print $ 3 `groupsOf` [1, 2, 3]
    print $ 3 `groupsOf` [1, 2, 3, 4]
    print $ 3 `groupsOf` [1, 2, 3, 4, 5]
    print $ 3 `groupsOf` [1, 2, 3, 4, 5, 6]
    print $ "Before"
    print $ tree
    print $ "Add 1"
    print $ (insertRect tree (Rect 0 0 2 2))
    print $ "Add 2"
    print $ insertRect (insertRect tree (Rect 0 0 2 2)) (Rect 0 0 5 5)
  where
    tree = Node [Node [] (Rect 0 0 20 20) 20, Node [] (Rect 20 20 40 40) 45] (Rect 0 0 40 40) 200
