module Graphics.UI.Oak.Basics where

import Control.Monad.Trans

data Size = Size Int Int
            deriving (Eq, Show)

increase :: Size -> Int -> Int -> Size
increase (Size w h) dw dh = Size (w + dw) (h + dh)

data Rect = Rect {
    rcX :: Int
  , rcY :: Int
  , rcSize :: Size
  } deriving (Eq, Show)

data Font = Font String Int
            deriving (Eq, Show)

data Key = ArrowLeft | ArrowUp | ArrowDown | ArrowRight
           | SpaceKey
           | F10
           | Return
             deriving (Eq, Show)

data Event = Quit
           | Live
           | KeyDown Key
             deriving (Eq, Show)

data Orientation = Horizontal | Vertical
                   deriving (Eq, Show)
