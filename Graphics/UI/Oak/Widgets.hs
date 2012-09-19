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

       , compact
       , hexpand
       , vexpand
         
       , vbox
       , hbox
       , table

       , vcenter
       , hcenter
       , center

       , margin

       , header
       , dialog
       ) where

import Data.Maybe (fromMaybe)

import Graphics.UI.Oak.Basics

class (Eq a, Show a) => Identifier a where
  unused       :: a
  btnBack      :: a


data MessageCode = Ok | Yes | No | Cancel
                   deriving (Eq, Show)

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
                | Table [[LayoutItem i m]]
                | Label String
                | Button String
                | Edit String Int
                | Stretch
                | Space Int
                | Line Int
                | Adjustable
                  (Maybe (SizePolicy, SizePolicy))
                  (Maybe (SizePolicy, SizePolicy))
                  (Widget i m)
                | Margin (Int, Int, Int, Int) (Widget i m)
                | Custom (WidgetBehavior m)
                  deriving (Eq, Show)

data SizePolicy = Fixed | Minimum | Expanding
                  deriving (Eq, Show)

compact :: Widget i m -> Widget i m
compact = Adjustable c c where c = Just (Minimum, Minimum)

hexpand :: Widget i m -> Widget i m
hexpand = Adjustable Nothing $ Just (Expanding, Expanding)

vexpand :: Widget i m -> Widget i m
vexpand = Adjustable (Just (Expanding, Expanding)) Nothing

vbox :: [(i, Widget i m)] -> Widget i m
vbox ws = VBox $ items ws

hbox :: [(i, Widget i m)] -> Widget i m
hbox ws = HBox $ items ws

table :: [[(i, Widget i m)]] -> Widget i m
table wss = Table $ map items wss

items :: [(i, Widget i m)] -> [LayoutItem i m]
items = map (\(i, w) -> LayoutItem i w (Rect 0 0 (Size 0 0)))


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
header s = compact $ vbox [ (unused, Label s)
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
isBox (HBox _)            = True
isBox (VBox _)            = True
isBox (Table _)           = True
isBox (Adjustable _ _ _)  = True
isBox (Margin _ _)        = True
isBox _                   = False

boxItems :: Widget i m -> [LayoutItem i m]
boxItems (HBox is)           = is
boxItems (VBox is)           = is
boxItems (Table iss)         = concat iss
boxItems (Adjustable _ _ w)  = boxItems w
boxItems (Margin _ w)        = boxItems w
boxItems _                   = []

acceptsFocus :: Monad m => Widget i m -> Bool
acceptsFocus (Button _)   = True
acceptsFocus (Edit _ _)   = True
acceptsFocus (Custom bh)  = accFocusFcn bh
acceptsFocus _            = False


-- Returns a (vertical, horizontal) size policies
sizePolicy :: Monad m =>
              Orientation -> Widget i m -> (SizePolicy, SizePolicy)
sizePolicy _ (VBox _)     = (Expanding, Minimum)
sizePolicy _ (HBox _)     = (Minimum,   Expanding)
sizePolicy _ (Table _)    = (Expanding, Expanding)
sizePolicy _ (Label _)    = (Minimum,   Minimum)
sizePolicy _ (Button _)   = (Minimum,   Minimum)
sizePolicy _ (Edit _ _ )  = (Minimum,   Expanding)
sizePolicy o (Space _)    = flexiblePolicy o
sizePolicy o (Line _)     = flexiblePolicy o
sizePolicy _ Stretch      = (Expanding, Expanding)
sizePolicy o (Margin _ w) = sizePolicy o w
sizePolicy o (Custom bh)  = sizePcyFcn bh o

sizePolicy o (Adjustable v h w)
  | o == Vertical   = fromMaybe orig v
  | o == Horizontal = fromMaybe orig h
  where orig = sizePolicy o w

flexiblePolicy :: Orientation -> (SizePolicy, SizePolicy)
flexiblePolicy o | o == Vertical   = (Fixed, Expanding)
                 | o == Horizontal = (Expanding, Fixed)
