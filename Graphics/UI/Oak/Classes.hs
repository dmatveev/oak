{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Graphics.UI.Oak.Classes
       (
         MonadSurface(..)
       , MonadFrontend(..)
       , MonadHandler(..)
       ) where

import Control.Monad.Trans

import Graphics.UI.Oak.Basics
import Graphics.UI.Oak.Widgets
import Graphics.UI.Oak.Utils (Stack)


class (Monad m) => MonadSurface m where
  textSize :: String -> m Size
  surfSize :: m Size

class (Monad m, MonadIO m) => MonadFrontend u m | m -> u where
  initialize :: m ()
  getEvents :: m [Event]
  ownData :: m u

  runFcn :: m (m a -> u -> IO (a, u))

  render :: Widget i m -> WidgetState -> Rect -> m ()
  endIter :: m ()

class (Monad m, MonadIO m, Identifier i, Eq i, Show i) =>
      MonadHandler i w mh m | m -> i, m -> mh, m -> w where
  hlift  :: mh a -> m a
  now    :: m Integer
  alter  :: i -> (Widget i mh -> Widget i mh) -> m ()
  open   :: Widget i mh -> m ()
  answer :: w -> m ()
  back   :: m ()
  quit   :: m ()

  msgBox :: String -> String -> [MessageCode] -> m (Maybe MessageCode)
  inputBox :: String -> String -> m (Maybe String)