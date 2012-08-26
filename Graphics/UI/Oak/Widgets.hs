module Graphics.UI.Oak.Widgets
       (
         Identifier(..)
       , DialogIdentifier(..)
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
  unused :: a

class DialogIdentifier a where
  backBtn :: a

data LayoutItem idt = LayoutItem {
    name     :: idt
  , widget   :: Widget idt
  , rect     :: Rect
  } deriving (Eq, Show)

data Widget idt = VBox [LayoutItem idt]
                | HBox [LayoutItem idt]
                | Label String
                | Button String
                | Stretch
                | Space Int
                | Line Int
                | Compact (Widget idt)
                deriving (Eq, Show)

data SizePolicy = Fixed | Minimum | Expanding
                  deriving (Eq, Show)

vbox :: [(idt, Widget idt)] -> Widget idt
vbox ws = VBox $ items ws

hbox :: [(idt, Widget idt)] -> Widget idt
hbox ws = HBox $ items ws

items :: [(idt, Widget idt)] -> [LayoutItem idt]
items = map (\(i, w) -> LayoutItem i w (Rect 0 0 (Size 0 0)))


margin :: Identifier i => Int -> (i, Widget i) -> Widget i
margin r p = vbox [ spc
                  , (unused, hbox [ spc, p, spc ])
                  , spc
                  ]
  where spc = (unused, Space r)

vcenter :: Identifier i => (i, Widget i) -> Widget i
vcenter p = vbox [ (unused, Stretch)
                 , p
                 , (unused, Stretch)
                 ]

hcenter :: Identifier i => (i, Widget i) -> Widget i
hcenter p = hbox [(unused, Stretch), p, (unused, Stretch)]

center :: Identifier i => (i, Widget i) -> Widget i
center p = vcenter (unused, hcenter p)

header :: Identifier i => String -> Widget i
header s = Compact $ vbox [ (unused, Label s)
                          , (unused, Line 3)
                          ]

dialog :: (Identifier i, DialogIdentifier i) =>
          String -> (i, Widget i) -> Widget i
dialog title contents =
  margin 20 (unused,
             vbox [ (unused, header title)
                  , (unused, margin 10 contents)
                  , (unused, hbox [ (backBtn, Button "Back"),
                                    (unused, Stretch)
                                  ]
                    )
                  ]
            )


isBox :: Widget i -> Bool
isBox (HBox _) = True
isBox (VBox _) = True
isBox _        = False

boxItems :: Widget i -> [LayoutItem i]
boxItems (HBox is) = is
boxItems (VBox is) = is
boxItems (Compact w) = boxItems w
boxItems _ = []

acceptsFocus :: Widget i -> Bool
acceptsFocus (VBox _)    = False
acceptsFocus (HBox _)    = False
acceptsFocus (Button _)  = True
acceptsFocus (Label _)   = False
acceptsFocus (Space _)   = False
acceptsFocus (Line _)    = False
acceptsFocus Stretch     = False
acceptsFocus (Compact _) = False

-- Returns a (vertical, horizontal) size policies
sizePolicy :: Orientation -> Widget idt -> (SizePolicy, SizePolicy)
sizePolicy _ (VBox _)    = (Expanding, Minimum)
sizePolicy _ (HBox _)    = (Minimum,   Expanding)
sizePolicy _ (Label _)   = (Minimum,   Minimum)
sizePolicy _ (Button _)  = (Minimum,   Minimum)
sizePolicy o (Space _)   = flexiblePolicy o
sizePolicy o (Line _)    = flexiblePolicy o
sizePolicy _ Stretch     = (Expanding, Expanding)
sizePolicy _ (Compact _) = (Minimum,   Minimum)

flexiblePolicy :: Orientation -> (SizePolicy, SizePolicy)
flexiblePolicy o | o == Vertical   = (Fixed, Expanding)
                 | o == Horizontal = (Expanding, Fixed)

data WidgetState = Normal | Focused
                   deriving (Eq, Show)
