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

findFirst :: (Widget i m -> Bool) -> Widget i m -> Maybe i
findFirst p w = findIn $ boxItems w
  where findIn items = mconcat $ map find $ map xtract items
        xtract litem = (name litem, widget litem)
        find (i, w) = if p w then Just i else findFirst p w


lookupWidget :: Eq i => i -> Widget i m -> Maybe (Widget i m)
lookupWidget i w = findIn $ boxItems w
  where findIn items = mconcat $ map find $ map xtract $ items
        xtract litem = (name litem, widget litem)
        find (i', w) = if i == i' then Just w else lookupWidget i w


updateInTree :: Eq i =>
                i -> (Widget i m -> Widget i m) -> Widget i m -> Widget i m
updateInTree i f w = case w of
    (HBox items)        -> HBox $ map upd items
    (VBox items)        -> VBox $ map upd items
    (Adjustable v h cw) -> Adjustable v h $ updateInTree i f cw
    (Margin m mw)       -> Margin m $ updateInTree i f mw
    (Table iss)         -> Table $ map (map upd) iss
    otherwise -> w
  where upd li@(LayoutItem n wgt _)
          | n == i    = modWidget li f
          | isBox wgt = setWidget li $ updateInTree i f wgt
          | otherwise = li       

genericNodesApply :: ((i, Widget i m) -> Bool)  -- filter predicate
                  -> ((i, Widget i m) -> a)     -- transform function
                  -> Widget i m                 -- the root widget
                  -> [a]
genericNodesApply p f wgt = concatMap (f' . xtract) $ boxItems wgt
  where xtract litem = (name litem, widget litem)
        f' t@(_, w) = let tail = genericNodesApply p f w
                      in if p t then f t : tail else tail


nodesApply :: ((i, Widget i m) -> a) -> Widget i m -> [a]
nodesApply = genericNodesApply $ const True


nodes :: Widget i m -> [(i, Widget i m)]
nodes = nodesApply id


leafsApply :: ((i, Widget i m) -> a) -> Widget i m -> [a]
leafsApply = genericNodesApply (\(_, w) -> not $ isBox w)


leafs :: Widget i m -> [(i, Widget i m)]
leafs = leafsApply id
