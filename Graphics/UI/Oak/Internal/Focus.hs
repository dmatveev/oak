module Graphics.UI.Oak.Internal.Focus
       (
         acceptsFocus
       , processFocus
       ) where

import Control.Applicative ((<$>))

import Graphics.UI.Oak.Basics
import Graphics.UI.Oak.Internal.Tree (genericNodesApply)
import Graphics.UI.Oak.Utils (nextAfter, prevBefore)
import Graphics.UI.Oak.Widgets (Widget, acceptsFocus, isBox)

processFocus :: (Eq i) => HandleResult -> Maybe i -> Widget i -> Maybe i
processFocus hr current root =
  if (hr == NextFocus || hr == PrevFocus)
  then let wgts = genericNodesApply filt fst root
           filt (_, w) = acceptsFocus w && (not $ isBox w)
           func = if hr == NextFocus then nextAfter else prevBefore
       in (\c -> func c wgts) <$> current
  else Nothing
