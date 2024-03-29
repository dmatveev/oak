{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Graphics.UI.Oak.Classes
       (
         MonadSurface(..)
       , MonadFrontend(..)
       , MonadHandler(..)
       , MonadUserState(..)
       ) where

import Control.Monad (liftM)
import Control.Monad.Trans

import Graphics.UI.Oak.Basics
import Graphics.UI.Oak.Widgets


class (Monad m) => MonadSurface m where
  textSize     :: String -> m Size
  surfSize     :: m Size
  buttonExtent :: m (Int, Int)
  editExtent   :: m (Int, Int)

class (Monad m, MonadIO m) => MonadFrontend u m | m -> u where
  initialize :: m ()
  getEvents :: m [Event]
  ownData :: m u

  runFcn :: m (m a -> u -> IO (a, u))

  render :: Widget i m -> WidgetState -> Rect -> m ()
  endIter :: m ()

class (Monad m, MonadIO m, Identifier i) =>
      MonadHandler i w mh m | m -> i, m -> mh, m -> w where
  hlift  :: mh a -> m a

  alter  :: i -> (Widget i mh -> Widget i mh) -> m ()
  open   :: Widget i mh -> m ()
  answer :: w -> m ()
  back   :: m ()
  quit   :: m ()

  lWidget :: i -> m (Maybe (Widget i mh))

  msgBox :: String -> String -> [MessageCode] -> m (Maybe MessageCode)
  inputBox :: String -> String -> m (Maybe String)


class (Monad m) => MonadUserState us m | m -> us where
  usGet   :: m us
  usPut   :: us -> m ()
  
  usGets  :: (us -> a) -> m a
  usGets f = usGet >>= \a -> return (f a)

  usMod   :: (us -> us) -> m ()
  usMod f = usGet >>= \a -> usPut (f a)

