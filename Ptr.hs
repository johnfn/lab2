module Ptr where

import Data.Lens.Common
import Control.Category
import Prelude hiding ((.))

-- Ptr isn't a pointer in the strict C/C++ sense of the word. But it may help
-- to think of Ptr as a level of dereference away from a Tree - able to walk
-- up and down and manipulate it without getting confused.

data Ptr a = Ptr [Int] (Int -> Lens a a)

newPtr :: (Int -> Lens a a) -> Ptr a
newPtr lens = (Ptr [] lens)

isRoot :: Ptr a -> Bool
isRoot (Ptr [] _) = True
isRoot _          = False

getChild :: Int -> Ptr a -> Ptr a
getChild which (Ptr list lens) = (Ptr (list ++ [which]) lens)

parent :: Ptr a -> Ptr a
parent (Ptr [] lens) = error "Tried to get parent of ptr at root."
parent (Ptr list lens) = Ptr (init list) lens

update :: Ptr a -> a -> (a -> a) -> a
update (Ptr list lensFn) root fn
    | list == [] = fn root
    | otherwise  = (accessor ^= (fn oldContent)) root
  where
    -- compose a list of lenses to the cursor
    lenses = map lensFn list
    accessor = foldl1 (.) lenses
    oldContent = root ^. accessor

deref :: a -> Ptr a -> a
deref root (Ptr list lensFn)
    | list == [] = root
    | otherwise  = root ^. accessor
  where
    lenses = map lensFn list
    accessor = foldl1 (.) lenses
