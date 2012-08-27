{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Graphics.UI.Oak.Classes
       (
         MonadSurface(..)
       , MonadFrontend(..)
       , MonadHandler(..)
       ) where

import Control.Monad.Trans

import Graphics.UI.Oak.Basics (Size, Rect, Event)
import Graphics.UI.Oak.Widgets (Widget, WidgetState)

class (Monad m) => MonadSurface m where
  textSize :: String -> m Size
  surfSize :: m Size

class (Monad m, MonadIO m) => MonadFrontend m where
  initialize :: m ()
  getEvents :: m [Event]

  render :: Widget i m -> WidgetState -> Rect -> m ()
  endIter :: m ()

class (Monad m, MonadIO m, Eq i) =>
      MonadHandler i mh m | m -> i, m -> mh where
  alter :: i -> (Widget i mh -> Widget i mh) -> m ()
  open  :: Widget i mh -> m ()
  back  :: m ()
  quit  :: m ()
  hlift :: mh a -> m a
