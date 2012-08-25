{-# LANGUAGE TemplateHaskell #-}

module Graphics.UI.Oak.Internal.Tree
       (
         findFirst
       , lookupWidget

       , updateInTree

       , genericNodesApply
       , nodesApply
       , nodes
       , leafsApply
       , leafs
       ) where

import Data.Mutators

import Graphics.UI.Oak.Basics
import Graphics.UI.Oak.Classes
import Graphics.UI.Oak.Widgets
import Graphics.UI.Oak.Utils (mconcat)

genMutators ''LayoutItem

findFirst :: (Widget i -> Bool) -> Widget i -> Maybe i
findFirst p w = findIn $ boxItems w
  where findIn items = mconcat $ map find $ map xtract items
        xtract litem = (name litem, widget litem)
        find (i, w) = if p w then Just i else findFirst p w


lookupWidget :: Eq i => i -> Widget i -> Maybe (Widget i)
lookupWidget i w = findIn $ boxItems w
  where findIn items = mconcat $ map find $ map xtract $ items
        xtract litem = (name litem, widget litem)
        find (i', w) = if i == i' then Just w else lookupWidget i w


updateInTree :: Eq i =>
                i -> (Widget i -> Widget i) -> Widget i -> Widget i
updateInTree i f w = case w of
    (HBox items) -> HBox $ map upd items
    (VBox items) -> VBox $ map upd items
    otherwise -> w
  where upd li@(LayoutItem n wgt _)
          | n == i    = modWidget li f
          | isBox wgt = setWidget li $ updateInTree i f wgt
          | otherwise = li       

genericNodesApply :: ((i, Widget i) -> Bool)  -- filter predicate
                  -> ((i, Widget i) -> a)     -- transform function
                  -> Widget i                 -- the root widget
                  -> [a]
genericNodesApply p f wgt = case wgt of
    (HBox items) -> concatMap (f' . xtract) items
    (VBox items) -> concatMap (f' . xtract) items
    otherwise    -> []
  where xtract litem = (name litem, widget litem)
        f' t@(_, w) = let tail = genericNodesApply p f w
                      in if p t then f t : tail else tail


nodesApply :: ((i, Widget i) -> a) -> Widget i -> [a]
nodesApply = genericNodesApply $ const True


nodes :: Widget i -> [(i, Widget i)]
nodes = nodesApply id


leafsApply :: ((i, Widget i) -> a) -> Widget i -> [a]
leafsApply = genericNodesApply (\(_, w) -> not $ isBox w)


leafs :: Widget i -> [(i, Widget i)]
leafs = leafsApply id
