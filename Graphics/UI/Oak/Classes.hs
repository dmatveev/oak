module Graphics.UI.Oak.Classes
       (
         MonadSurface(..)
       , MonadFrontend(..)
       ) where

import Control.Monad.Trans (MonadIO)

import Graphics.UI.Oak.Basics (Size, Rect, Event)
import Graphics.UI.Oak.Widgets (Widget, WidgetState)


class (Monad m) => MonadSurface m where
  textSize :: String -> m Size
  surfSize :: m Size

class (Monad m, MonadIO m) => MonadFrontend m where
  initialize :: m ()
  getEvents :: m [Event]

  render :: Widget idt -> WidgetState -> Rect -> m ()
  endIter :: m ()

