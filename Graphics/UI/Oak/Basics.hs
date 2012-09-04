module Graphics.UI.Oak.Basics where

import Control.Monad.Trans

data Size = Size { szWidth :: Int, szHeight :: Int }
            deriving (Eq, Show)

increase :: Size -> Int -> Int -> Size
increase (Size w h) dw dh = Size (w + dw) (h + dh)

decrease :: Size -> Int -> Int -> Size
decrease sz dw dh = increase sz (negate dw) (negate dh)

adjust :: Rect -> (Int, Int, Int, Int) -> Rect
adjust rc@(Rect x y sz) (l, t, b, r) =
  Rect (x - l) (y - t) (increase sz (l + r) (t + b))

nadjust :: Rect -> (Int, Int, Int, Int) -> Rect
nadjust rc (l, t, b, r) =
  adjust rc (negate l, negate t, negate b, negate r)


data Rect = Rect {
    rcX    :: Int
  , rcY    :: Int
  , rcSize :: Size
  } deriving (Eq, Show)

data Font = Font String Int
            deriving (Eq, Show)

data Key = ArrowLeft | ArrowUp | ArrowDown | ArrowRight
           | Character Char
           | Home | End
           | Delete | Backspace
           | F10
           | Return
           | SpaceKey
           | Tab
             deriving (Eq, Show)

data Event = Quit
           | Live
           | KeyDown Key
             deriving (Eq, Show)

data Orientation = Horizontal | Vertical
                   deriving (Eq, Show)
