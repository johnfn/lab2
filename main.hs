
import Rect
import Data.Maybe
import Data.Ord
import Data.Lens.Common
import Control.Category
import Prelude hiding ((.))
import Data.List
import Rect
import Ptr

data HTree = Node { _entries :: [HTree]
                  , _rect    :: Rect
                  , _lhv     :: Int
                  } deriving (Show, Eq)

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
-- TODO - instead of saying "i want grps of size 3" easier to say "i want 3 groups" also allows for sexy 3 `groupsOf` xx infixity
groupsOf :: Int -> [a] -> [[a]]
groupsOf size list = groupsOf' list size size [[]]

groupsOf' :: [a] -> Int -> Int -> [[a]] -> [[a]]
groupsOf' list cur size result
  | null list = result
  | cur == 0 = groupsOf'  (tail list) (size - 1) size (result ++ [[head list]])
  | otherwise = groupsOf' (tail list) (cur - 1)  size ((init result) ++ [last result ++ [head list]])


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
distributeEntriesToNodes (grp:grps) (sib:sibs) root = 
    distributeEntriesToNodes grps sibs newRoot
  where
    newRoot = setNode sib root grp

handleOverflow :: Ptr HTree -> Rect -> HTree -> (Maybe HTree, HTree)
handleOverflow node rect root =
    if allFull
      then let updatedRoot = distributeEntriesToNodes (tail entryGroups) (getSiblings node root) root
               --TODO actual bounding thing for group
               splitNode   = Node (head entryGroups) rect (hilbertValue rect) in
                 (Just splitNode, updatedRoot)
      else let updatedRoot = distributeEntriesToNodes entryGroups (getSiblings node root) root in
        (Nothing, updatedRoot)
  where
    newEntry = Node [] rect (hilbertValue rect)
    entries :: [HTree] = concat $ (map _entries $ (map (deref root) (getSiblings node root)))
    entryGroups :: [[HTree]] = groupsOf 3 entries --TODO - not 3
    allFull = all full (map (deref root) (getSiblings node root))

--TODO - i dont use siblings (var) here.

--createdNode => split
adjustTree :: Ptr HTree -> Maybe [Ptr HTree] -> Maybe HTree -> HTree -> HTree
adjustTree updatedNode siblings createdNode root =
    if isRoot updatedNode
      then root
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
            let newNode = fromJust createdNode in    
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
    print $ "Before"
    print $ tree
    {-
    print $ "Add 1"
    print $ (insertRect tree (Rect 0 0 2 2))
    print $ "Add 2"
    print $ insertRect (insertRect tree (Rect 0 0 2 99999992)) (Rect 0 0 5 5)
    -}
  where
    tree = Node [Node [] (Rect 0 0 20 20) 20, Node [] (Rect 20 20 40 40) 45] (Rect 0 0 40 40) 200
