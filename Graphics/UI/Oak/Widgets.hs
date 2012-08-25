module Graphics.UI.Oak.Widgets
       (
         LayoutItem(..)
       , Widget(..)
       , WidgetState(..)
       , SizePolicy(..)

       , acceptsFocus
       , isBox
       , sizePolicy

       , vbox
       , hbox
       ) where

import Graphics.UI.Oak.Basics

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
                deriving (Eq, Show)

data SizePolicy = Minimum | Expanding
                  deriving (Eq, Show)

vbox :: [(idt, Widget idt)] -> Widget idt
vbox ws = VBox $ items ws

hbox :: [(idt, Widget idt)] -> Widget idt
hbox ws = HBox $ items ws

items :: [(idt, Widget idt)] -> [LayoutItem idt]
items = map (\(i, w) -> LayoutItem i w (Rect 0 0 (Size 0 0)))

isBox :: Widget i -> Bool
isBox (HBox _) = True
isBox (VBox _) = True
isBox _        = False

acceptsFocus :: Widget i -> Bool
acceptsFocus (VBox _)   = False
acceptsFocus (HBox _)   = False
acceptsFocus (Button _) = True
acceptsFocus (Label _)  = False
acceptsFocus Stretch    = False

-- Returns a (vertical, horizontal) size policies
sizePolicy :: Widget idt -> (SizePolicy, SizePolicy)
sizePolicy (VBox _)   = (Expanding, Minimum)
sizePolicy (HBox _)   = (Minimum,   Expanding)
sizePolicy (Label _)  = (Minimum,   Minimum)
sizePolicy (Button _) = (Minimum,   Minimum)
sizePolicy Stretch    = (Expanding, Expanding)



data WidgetState = Normal | Focused
                 deriving (Eq, Show)

  