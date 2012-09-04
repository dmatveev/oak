{-# LANGUAGE TemplateHaskell #-}

module Graphics.UI.Oak.Internal.Layout
       (
         updateLayout
       ) where

import Control.Monad (forM, liftM)
import Data.Mutators
import Data.List (foldl', find)

import Graphics.UI.Oak.Basics
import Graphics.UI.Oak.Classes
import Graphics.UI.Oak.Widgets

genMutators ''LayoutItem

sizePolicy' :: Monad m =>
               Orientation -> LayoutItem i m -> (SizePolicy, SizePolicy)
sizePolicy' o (LayoutItem _ w _) = sizePolicy o w


sizeHint :: (MonadSurface m) => Orientation -> Widget i m -> m Size
sizeHint _ (Label s)  = textSize s
sizeHint _ (Button s) = do sz <- textSize s
                           return $ increase sz 5 5
sizeHint _ (Edit s _) = do sz <- textSize (if null s then " " else s)
                           return $ increase sz 7 7

sizeHint _ Stretch = return $ Size 0 0

sizeHint _ (VBox items) = do
  sizes <- mapM (getSizeHint Vertical) items 
  let sz@(Size maxW _) = maxSize sizes
      totalH = foldl' (\acc (Size _ h) -> acc + h) 0 sizes
  return $ Size maxW totalH

sizeHint _ (HBox items) = do
  sizes <- mapM (getSizeHint Horizontal) items
  let (Size _ maxH) = maxSize sizes
      totalW = foldl' (\acc (Size w _) -> acc + w) 0 sizes
  return $ Size totalW maxH

sizeHint o (Space s)   = return $ flexibleSize o s
sizeHint o (Line n)    = return $ flexibleSize o n
sizeHint o (Compact w) = sizeHint o w

sizeHint o (Margin (l, t, r, b) w) = do
  sh <- sizeHint o w
  return $ increase sh (l + r) (t + b)

sizeHint o (Custom bh) = sizeHintFcn bh o

flexibleSize :: Orientation -> Int -> Size
flexibleSize o s | o == Vertical   = Size 0 s
                 | o == Horizontal = Size s 0


maxSize :: [Size] -> Size
maxSize = foldl' maxSz (Size 0 0)
  where maxSz (Size a b) (Size c d) = Size (max a c) (max b d)


getSizeHint :: (MonadSurface m) => Orientation -> LayoutItem i m -> m Size
getSizeHint o (LayoutItem _ wgt _) = sizeHint o wgt


is :: SizePolicy -> (LayoutItem i m, Size, SizePolicy) -> Bool
is p (_, _, pcy) = pcy == p

totalLen :: (Size -> Int) -> [(LayoutItem i m, Size, SizePolicy)] -> Int
totalLen acc = foldl' cnt 0 where cnt t (_, sz, _) = t + acc sz


calcBoxLayout :: Eq i
                 => [(LayoutItem i m, Size, SizePolicy)] -- subject widgets and its data
                 -> Int                                  -- base axis value
                 -> Int                                  -- available size element
                 -> (Size -> Int)                        -- counted size element accessor
                 -> (Int -> Int -> Rect)                 -- rect builder function
                 -> [LayoutItem i m]
calcBoxLayout items base availLen cntAcc buildRect =
    let fixs = filter (is Fixed)     items
        mins = filter (is Minimum)   items
        exps = filter (is Expanding) items
        rects =
          if null exps
          then
            -- No Expanding elements, divide the available area
            -- between the (Minimum) items
            let minlen  = (availLen - totalLen cntAcc fixs) `div` length mins
                lensfcn = (\(_, sz, p) -> if p == Fixed then cntAcc sz else minlen)
                lens    = map lensfcn items
                axs     = scanl (+) base lens
            in map (\(offset, len) -> buildRect offset len) $ zip axs lens
          else
            -- Calculate size required for the items with the
            -- Minimum and Fixed policies, divide the rest between the
            -- Expanding elements
            let rqLen = totalLen cntAcc $ mins ++ fixs
                exLen = div (availLen - rqLen) $ length exps
                ols = reverse
                      $ fst
                      $ foldl' (accumOLs exLen) ([], base)
                      $ items
            in map (\(offset, len) -> buildRect offset len) ols
      in map (\(rc, (li, _, _)) -> setRect li rc) $ zip rects items
  where
    accumOLs exLen (ols, offset) (_, sz, pcy) =
      let len = if pcy == Expanding then exLen else cntAcc sz
      in ((offset, len) : ols, offset + len)


updateLayouts :: (MonadSurface m, Eq i)
                 => [LayoutItem i m]
                 -> m [LayoutItem i m]
updateLayouts items =
  forM items $ \(LayoutItem i w rc@(Rect x y sz)) -> do
    w' <- updateLayout w x y sz
    return $ LayoutItem i w' rc
  

updateLayout :: (MonadSurface m, Eq i)
                => Widget i m
                -> Int
                -> Int
                -> Size
                -> m (Widget i m)

updateLayout (VBox items) baseX baseY (Size availW availH) = do
  sizes <- mapM (getSizeHint Vertical) items
  let policies = map fst $ map (sizePolicy' Vertical) items
      calcInfo = zip3 items sizes policies
      szHeight (Size _ h) = h
      bldRC y height = Rect baseX y $ Size availW height
      calcd = calcBoxLayout calcInfo baseY availH szHeight bldRC
  items' <- updateLayouts calcd
  return $ VBox items'

updateLayout (HBox items) baseX baseY (Size availW availH) = do
  sizes <- mapM (getSizeHint Horizontal) items
  let policies = map snd $ map (sizePolicy' Horizontal) items
      calcInfo = zip3 items sizes policies
      szWidth (Size w _) = w
      bldRC x width = Rect x baseY $ Size width availH
      calcd = calcBoxLayout calcInfo baseX availW szWidth bldRC
  items' <- updateLayouts calcd
  return $ HBox items'

updateLayout (Compact cw) x y sz = liftM Compact $ updateLayout cw x y sz

updateLayout (Margin m@(l, t, r, b) w) x y sz = do
  let newSize = decrease sz (l + r) (t + b)
  liftM (Margin m) $ updateLayout w (x + l) (y + t) newSize

updateLayout w _ _ _ = return w
