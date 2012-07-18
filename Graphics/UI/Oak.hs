{-# LANGUAGE TemplateHaskell #-}

module Graphics.UI.Oak
       (
         Widget(..)
       , Size(..)

       , vbox
       , hbox

         -- drop after debugging!
       , updateLayout
       , sizeHint
       )where

import Data.List (foldl', find)
import Data.Mutators
import Control.Monad (mapM, forM)

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


genMutators ''LayoutItem


vbox :: [(idt, Widget idt)] -> Widget idt
vbox ws = VBox $ items ws


hbox :: [(idt, Widget idt)] -> Widget idt
hbox ws = HBox $ items ws


items :: [(idt, Widget idt)] -> [LayoutItem idt]
items = map (\(i, w) -> LayoutItem i w (Rect 0 0 (Size 0 0)))


maxSize :: [Size] -> Size
maxSize = foldl' maxSizeOf (Size 0 0)
  where maxSizeOf (Size a b) (Size c d) = Size (max a b) (max c d)

-- Returns a (vertical, horizontal) size policies
sizePolicy :: Widget idt -> (SizePolicy, SizePolicy)
sizePolicy (VBox _)   = (Expanding, Minimum)
sizePolicy (HBox _)   = (Minimum,   Expanding)
sizePolicy (Label _)  = (Minimum,   Minimum)
sizePolicy (Button _) = (Minimum,   Minimum)
sizePolicy Stretch    = (Expanding, Expanding)


sizeHint :: (MonadSurface m) => Widget idt -> m Size
sizeHint (Label s)  = textSize s
sizeHint (Button s) = textSize s >>= (\sz -> return $ increase sz 5 5)
sizeHint Stretch = return $ Size 0 0

sizeHint (VBox items) = do
  sizes <- mapM getSizeHint items
  let (Size maxW _) = maxSize sizes
      totalH = foldl' (\acc (Size _ h) -> acc + h) 0 sizes
  return $ Size maxW totalH

sizeHint (HBox items) = do
  sizes <- mapM getSizeHint items
  let (Size _ maxH) = maxSize sizes
      totalW = foldl' (\acc (Size w _) -> acc + w) 0 sizes
  return $ Size totalW maxH

getSizeHint :: (MonadSurface m) => LayoutItem idt -> m Size
getSizeHint (LayoutItem _ w _) = sizeHint w


calcBoxLayout :: Eq idt
                 => [(LayoutItem idt, Size, SizePolicy)] -- subject widgets and its data
                 -> Int                                  -- base axis value
                 -> Int                                  -- available size element (divided between items)
                 -> (Size -> Int)                        -- counted size element accessor
                 -> (Int -> Int -> Rect)                 -- rect builder functionx
                 -> [LayoutItem idt]
calcBoxLayout items base availLen cntAcc buildRect = do
    let rects = if find isExpanding items == Nothing
                then
                  -- No Expanding elements, divide the available area
                  -- between the items
                  let len = availLen `div` length items
                      axs = [base, base + len..]
                  in map (\offset -> buildRect offset len) axs
                else
                  -- Calculate size required for the items with the
                  -- Minimum policy, divide the rest between the
                  -- Expanding elements
                  let mins = filter isMinimum items
                      exps = filter isExpanding items
                      rqLen = foldl' (\acL (_, sz, _) -> acL + cntAcc sz) 0 mins
                      exLen = div (availLen - rqLen) $ length exps
                      ols = reverse
                            $ fst
                            $ foldl' (accumOLs exLen) ([], base)
                            $ items
                  in map (\(offset, len) -> buildRect offset len) ols
      in map (\(rc, (li, _, _)) -> setRect li rc) $ zip rects items

  where
    isMinimum   (_, _, pcy) = pcy == Minimum
    isExpanding (_, _, pcy) = pcy == Expanding

    accumOLs exLen (pls, offset) (_, sz, pcy) =
      let len = if pcy == Expanding then exLen else cntAcc sz
      in ((offset, len) : pls, offset + len)


sizePolicy' :: LayoutItem idt -> (SizePolicy, SizePolicy)
sizePolicy' (LayoutItem _ w _) = sizePolicy w


updateLayouts :: (MonadSurface m, Eq idt)
                 => [LayoutItem idt]
                 -> m [LayoutItem idt]
updateLayouts items = do
  updated <- forM items $ \(LayoutItem i w rc@(Rect x y sz)) -> do
    w' <- updateLayout w x y sz
    return $ LayoutItem i w' rc
  return updated
  

updateLayout :: (MonadSurface m, Eq idt)
                => Widget idt
                -> Int
                -> Int
                -> Size
                -> m (Widget idt)

updateLayout (VBox items) baseX baseY (Size availW availH) = do
  sizes <- mapM getSizeHint items
  let policies = map fst $ map sizePolicy' items
      calcInfo = zip3 items sizes policies
      szHeight (Size _ h) = h
      bldRC y height = Rect baseX y $ Size availW height
      calcd = calcBoxLayout calcInfo baseY availH szHeight bldRC
  items' <- updateLayouts calcd
  return $ VBox items'

updateLayout (HBox items) baseX baseY (Size availW availH) = do
  sizes <- mapM getSizeHint items
  let policies = map snd $ map sizePolicy' items
      calcInfo = zip3 items sizes policies
      szWidth (Size w _) = w
      bldRC x width = Rect x baseY $ Size width availH
      calcd = calcBoxLayout calcInfo baseX availW szWidth bldRC
  items' <- updateLayouts calcd
  return $ HBox items'

updateLayout w _ _ _ = return w
