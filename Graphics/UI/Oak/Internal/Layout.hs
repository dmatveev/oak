{-# LANGUAGE TemplateHaskell #-}

module Graphics.UI.Oak.Internal.Layout
       (
         updateLayout
       ) where

import Control.Monad (forM, liftM)
import Data.Mutators
import Data.List (foldl', find, transpose)

import Graphics.UI.Oak.Basics
import Graphics.UI.Oak.Classes
import Graphics.UI.Oak.Widgets
import Graphics.UI.Oak.Utils (mapmap, mapmapM, for)

genMutators ''LayoutItem

sizePolicy' :: Monad m
               => Orientation
               -> LayoutItem i m
               -> (SizePolicy, SizePolicy)
sizePolicy' o (LayoutItem _ w _) = sizePolicy o w


sizeHint :: (MonadSurface m) => Orientation -> Widget i m -> m Size
sizeHint _ (Label s)  = textSize s
sizeHint _ (Button s) = do sz <- textSize s
                           (dw, dh) <- buttonExtent
                           return $ increase sz dw dh
sizeHint _ (Edit s _) = do sz <- textSize (if null s then " " else s)
                           (dw, dh) <- editExtent
                           return $ increase sz dw dh
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

sizeHint o (Table iss) = do
  sizes <- mapM (mapM $ getSizeHint o) iss
  let widths  = map (sum . map szWidth)  sizes
      heights = map (sum . map szHeight) $ transpose sizes
  return $ Size (maximum widths) (maximum heights)

sizeHint o (Space s)          = return $ flexibleSize o s
sizeHint o (Line n)           = return $ flexibleSize o n
sizeHint o (Adjustable _ _ w) = sizeHint o w

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


getSizeHint :: (MonadSurface m) =>
               Orientation -> LayoutItem i m -> m Size
getSizeHint o (LayoutItem _ wgt _) = sizeHint o wgt

 
updateLayouts :: (MonadSurface m, Eq i)
                 => [LayoutItem i m]
                 -> m [LayoutItem i m]
updateLayouts items =
  forM items $ \(LayoutItem i w rc@(Rect x y sz)) -> do
    w' <- updateLayout w x y sz
    return $ LayoutItem i w' rc
  

calcLens :: [(Int, SizePolicy)] -> Int -> [Int]
calcLens lpcs avail =
    let fixs = filter (is Fixed)     lpcs
        mins = filter (is Minimum)   lpcs
        exps = filter (is Expanding) lpcs
    in if null exps
       then
         -- No Expanding elements, divide the available area
         -- between the (Minimum) items
         let minlen = (avail - len fixs) `div` length mins
             lfcn (l, p) = if p == Fixed then l else minlen
         in map lfcn lpcs
       else
         -- Calculate size required for the items with the
         -- Minimum and Fixed policies, divide the rest between the
         -- Expanding elements
         let rqlen       = len $ mins ++ fixs
             exlen       = div (avail - rqlen) $ length exps
             lfcn (l, p) = if p == Expanding then exlen else l
         in map lfcn lpcs
  where is p (_, pcy) = pcy == p
        len items = sum $ map fst items
       

updateBoxLayout :: (MonadSurface m, Eq i)
                => [LayoutItem i m]
                -> Int
                -> Int
                -> Orientation
                -> (Size -> Int)
                -> ((SizePolicy, SizePolicy) -> SizePolicy)
                -> (Int -> Int -> Rect)
                -> m [LayoutItem i m]
updateBoxLayout items base avail ori accSz accPcy buildRect = do
  hints <- mapM (liftM accSz . getSizeHint ori) items
  let pcys  = map accPcy $ map (sizePolicy ori . widget) items
      lens  = calcLens (zip hints pcys) avail
      offs  = scanl (+) base lens
      calcd = for (zip3 offs lens items) $ \(o, l, item) ->
        setRect item $ buildRect o l
  updateLayouts calcd >>= return
  

updateLayout :: (MonadSurface m, Eq i)
                => Widget i m
                -> Int
                -> Int
                -> Size
                -> m (Widget i m)

updateLayout (VBox items) baseX baseY (Size availW availH) =
    updateBoxLayout items baseY availH Vertical szHeight fst bRect >>= (return . VBox)
  where bRect o l = Rect baseX o $ Size availW l

updateLayout (HBox items) baseX baseY (Size availW availH) = do
    updateBoxLayout items baseX availW Horizontal szWidth snd bRect >>= (return . HBox)
  where bRect o l = Rect o baseY $ Size l availH

updateLayout (Table iss) baseX baseY (Size availW availH) = do
    whints <- mapmapM (liftM szWidth  . getSizeHint Horizontal) iss
    hhints <- mapmapM (liftM szHeight . getSizeHint Vertical)   iss

    let vpcys = mapmap (fst . sizePolicy Vertical   . widget) iss
        hpcys = mapmap (snd . sizePolicy Horizontal . widget) iss
        rpcys = map inferPolicy vpcys
        cpcys = map inferPolicy $ transpose hpcys

        whints' = map maximum $ transpose whints
        hhints' = map maximum hhints

        widths  = calcLens (zip whints' cpcys) availW
        heights = calcLens (zip hhints' rpcys) availH
        xs      = scanl (+) baseX widths
        ys      = scanl (+) baseY heights
        
        calcd   = for (zip3 ys heights iss) $ \(y, h, is) ->
          for (zip3 xs widths is) $ \(x, w, li) ->
            setRect li $ Rect x y $ Size w h
            
    mapM updateLayouts calcd >>= (return . Table)

  where inferPolicy pcys | Expanding `elem` pcys = Expanding
                         | all (== Fixed) pcys   = Fixed
                         | otherwise             = Minimum

updateLayout (Adjustable v h w) x y sz =
  liftM (Adjustable v h) $ updateLayout w x y sz

updateLayout (Margin m@(l, t, r, b) w) x y sz = do
  let newSize = decrease sz (l + r) (t + b)
  liftM (Margin m) $ updateLayout w (x + l) (y + t) newSize

updateLayout w _ _ _ = return w
