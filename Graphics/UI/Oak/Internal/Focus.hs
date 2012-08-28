module Graphics.UI.Oak.Internal.Focus
       (
         nextFocus
       , prevFocus
       ) where

import Control.Applicative ((<$>))

import Graphics.UI.Oak.Basics
import Graphics.UI.Oak.Internal.Tree (genericNodesApply)
import Graphics.UI.Oak.Utils (nextAfter, prevBefore)
import Graphics.UI.Oak.Widgets (Widget, acceptsFocus, isBox)


nextFocus :: (Eq i, Monad m) =>
             Maybe i -> Widget i m -> Maybe i
nextFocus c r = processFocus nextAfter c r

prevFocus :: (Eq i, Monad m) =>
             Maybe i -> Widget i m -> Maybe i
prevFocus c r = processFocus prevBefore c r

processFocus :: (Eq i, Monad m) =>
                (i -> [i] -> i) -> Maybe i -> Widget i m -> Maybe i
processFocus selectorFunc current root =
  let wgts = genericNodesApply filt fst root
      filt (_, w) = acceptsFocus w && (not $ isBox w)
  in (\c -> selectorFunc c wgts) <$> current
