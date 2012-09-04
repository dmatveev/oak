module Graphics.UI.Oak.Widgets
       (
         Identifier(..)
       , MessageCode(..)

       , WidgetBehavior(..)
         
       , LayoutItem(..)
       , Widget(..)
       , WidgetState(..)
       , SizePolicy(..)

       , acceptsFocus
       , isBox
       , boxItems
       , sizePolicy

       , vbox
       , hbox

       , vcenter
       , hcenter
       , center

       , margin

       , header
       , dialog
       ) where

import Graphics.UI.Oak.Basics


class Identifier a where
  unused       :: a
  btnBack      :: a


data WidgetState = Normal | Focused
                   deriving (Eq, Show)

data LayoutItem i m = LayoutItem {
    name     :: i
  , widget   :: Widget i m
  , rect     :: Rect
  } deriving (Eq, Show)

data (Monad m) => WidgetBehavior m = WidgetBehavior {
    accFocusFcn :: Bool
  , sizePcyFcn  :: Orientation -> (SizePolicy, SizePolicy)
  , sizeHintFcn :: Orientation -> m Size
  , renderFcn   :: WidgetState -> Rect -> m ()
  }

instance Show (WidgetBehavior m) where
  show _ = "<#behavior>"

instance Eq (WidgetBehavior m) where
  _ == _ = False

data Widget i m = VBox [LayoutItem i m]
                | HBox [LayoutItem i m]
                | Label String
                | Button String
                | Edit String Int
                | Stretch
                | Space Int
                | Line Int
                | Compact (Widget i m)
                | Margin (Int, Int, Int, Int) (Widget i m)
                | Custom (WidgetBehavior m)
                  deriving (Eq, Show)

data SizePolicy = Fixed | Minimum | Expanding
                  deriving (Eq, Show)

data MessageCode = Ok | Yes | No | Cancel
                   deriving (Eq)

vbox :: [(i, Widget i m)] -> Widget i m
vbox ws = VBox $ items ws

hbox :: [(i, Widget i m)] -> Widget i m
hbox ws = HBox $ items ws

items :: [(i, Widget i m)] -> [LayoutItem i m]
items = map (\(i, w) -> LayoutItem i w (Rect 0 0 (Size 0 0)))


-- margin :: Identifier i => Int -> (i, Widget i m) -> Widget i m
-- margin r p = vbox [ spc
--                   , (unused, hbox [ spc, p, spc ])
--                   , spc
--                   ]
--   where spc = (unused, Space r)

margin :: Identifier i => Int -> (i, Widget i m) -> Widget i m
margin m (_, w) = Margin (m, m, m, m) w

vcenter :: Identifier i => (i, Widget i m) -> Widget i m
vcenter p = vbox [ (unused, Stretch)
                 , p
                 , (unused, Stretch)
                 ]

hcenter :: Identifier i => (i, Widget i m) -> Widget i m
hcenter p = hbox [(unused, Stretch), p, (unused, Stretch)]

center :: Identifier i => (i, Widget i m) -> Widget i m
center p = vcenter (unused, hcenter p)

header :: Identifier i => String -> Widget i m
header s = Compact $ vbox [ (unused, Label s)
                          , (unused, Line 3)
                          ]

dialog :: Identifier i
          => String
          -> [(i, Widget i m)]
          -> (i, Widget i m)
          -> Widget i m
dialog title buttons contents =
  margin 20 (unused,
             vbox [ (unused, header title)
                  , (unused, margin 10 contents)
                  , (unused, hbox $ buttons ++ [(unused, Stretch)])
                  ]
            )



isBox :: Widget i m -> Bool
isBox (HBox _)     = True
isBox (VBox _)     = True
isBox (Compact _)  = True
isBox (Margin _ _) = True
isBox _            = False

boxItems :: Widget i m -> [LayoutItem i m]
boxItems (HBox is)    = is
boxItems (VBox is)    = is
boxItems (Compact w)  = boxItems w
boxItems (Margin _ w) = boxItems w
boxItems _ = []

acceptsFocus :: Monad m => Widget i m -> Bool
acceptsFocus (VBox _)     = False
acceptsFocus (HBox _)     = False
acceptsFocus (Label _)    = False
acceptsFocus (Button _)   = True
acceptsFocus (Edit _ _)   = True
acceptsFocus (Space _)    = False
acceptsFocus (Line _)     = False
acceptsFocus Stretch      = False
acceptsFocus (Compact _)  = False
acceptsFocus (Margin _ _) = False
acceptsFocus (Custom bh)  = accFocusFcn bh

-- Returns a (vertical, horizontal) size policies
sizePolicy :: Monad m =>
              Orientation -> Widget i m -> (SizePolicy, SizePolicy)
sizePolicy _ (VBox _)     = (Expanding, Minimum)
sizePolicy _ (HBox _)     = (Minimum,   Expanding)
sizePolicy _ (Label _)    = (Minimum,   Minimum)
sizePolicy _ (Button _)   = (Minimum,   Minimum)
sizePolicy _ (Edit _ _ )  = (Minimum,   Expanding)
sizePolicy o (Space _)    = flexiblePolicy o
sizePolicy o (Line _)     = flexiblePolicy o
sizePolicy _ Stretch      = (Expanding, Expanding)
sizePolicy _ (Compact _)  = (Minimum,   Minimum)
sizePolicy o (Margin _ w) = sizePolicy o w
sizePolicy o (Custom bh)  = sizePcyFcn bh o

flexiblePolicy :: Orientation -> (SizePolicy, SizePolicy)
flexiblePolicy o | o == Vertical   = (Fixed, Expanding)
                 | o == Horizontal = (Expanding, Fixed)
