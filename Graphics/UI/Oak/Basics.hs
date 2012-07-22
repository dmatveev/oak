module Graphics.UI.Oak.Basics where

import Control.Monad.Trans

data Size = Size Int Int
            deriving (Eq, Show)

data SizePolicy = Minimum | Expanding
                  deriving (Eq, Show)

increase :: Size -> Int -> Int -> Size
increase (Size w h) dw dh = Size (w + dw) (h + dh)


data Rect = Rect {
    rcX :: Int
  , rcY :: Int
  , rcSize :: Size
  } deriving (Eq, Show)


class (Monad m) => MonadSurface m where
  textSize :: String -> m Size
  surfSize :: m Size

  
class (Monad m, MonadIO m) => MonadFrontend m where
  initialize :: m ()
