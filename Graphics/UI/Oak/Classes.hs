{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Graphics.UI.Oak.Classes
       (
         MonadSurface(..)
       , MonadFrontend(..)
       , MonadHandler(..)
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

  render :: Widget i -> WidgetState -> Rect -> m ()
  endIter :: m ()

class (Monad m, MonadIO m, Eq i) => MonadHandler i m | m -> i where
  alter :: i -> (Widget i -> Widget i) -> m ()
  open :: Widget i -> m ()
  back :: m ()
  quit :: m ()