{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Graphics.UI.Oak
       (
         Widget(..)
       , Size(..)
       , Font(..)

       , vbox
       , hbox

       , runOak

         -- drop after debugging!
       , updateLayout
       , sizeHint
       )where

import Data.List (foldl', find)
import Data.Mutators
import Control.Monad (mapM, forM)
import Control.Monad.State
import Control.Monad.Trans

import Graphics.UI.Oak.Basics


genMutators ''LayoutItem


vbox :: [(idt, Widget idt)] -> Widget idt
vbox ws = VBox $ items ws


hbox :: [(idt, Widget idt)] -> Widget idt
hbox ws = HBox $ items ws


items :: [(idt, Widget idt)] -> [LayoutItem idt]
items = map (\(i, w) -> LayoutItem i w (Rect 0 0 (Size 0 0)))


maxSize :: [Size] -> Size
maxSize = foldl' maxSizeOf (Size 0 0)
  where maxSizeOf (Size a b) (Size c d) = Size (max a c) (max b d)

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
  let sz@(Size maxW _) = maxSize sizes
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
                 -> (Int -> Int -> Rect)                 -- rect builder function
                 -> [LayoutItem idt]
calcBoxLayout items base availLen cntAcc buildRect = do
    let rects =
          if find isExpanding items == Nothing
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

    accumOLs exLen (ols, offset) (_, sz, pcy) =
      let len = if pcy == Expanding then exLen else cntAcc sz
      in ((offset, len) : ols, offset + len)


sizePolicy' :: LayoutItem idt -> (SizePolicy, SizePolicy)
sizePolicy' (LayoutItem _ w _) = sizePolicy w


updateLayouts :: (MonadSurface m, Eq idt)
                 => [LayoutItem idt]
                 -> m [LayoutItem idt]
updateLayouts items =
  forM items $ \(LayoutItem i w rc@(Rect x y sz)) -> do
    w' <- updateLayout w x y sz
    return $ LayoutItem i w' rc
  

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




data Display idt = Display {
    root    :: Widget idt
  , focused :: Maybe idt
  } deriving (Eq, Show)

genMutators ''Display

newtype MonadFrontend m =>
        OakT i m a = OakT (StateT (Display i) m a)
                     deriving ( Monad
                              , MonadIO
                              , MonadTrans
                              , MonadState (Display i)
                              )

runOakT :: MonadFrontend m => OakT i m a -> Display i -> m a
runOakT (OakT stt) d = evalStateT stt d


repaint :: (MonadFrontend m, MonadSurface m) => OakT i m ()
repaint = do
  wgt <- gets root
  sz <- lift $ surfSize
  lift $ render wgt $ Rect 0 0 sz
  

eventLoop :: (MonadFrontend m, MonadSurface m) => OakT i m ()
eventLoop = do
  events <- lift $ getEvents
  if any (== Quit) events
    then return ()
    else do repaint
            lift endIter
            eventLoop


recalcLayout :: (MonadSurface m, Eq i, Show i) => OakT i m ()
recalcLayout = do
  thisRoot <- gets root
  size <- lift $ surfSize
  newRoot <- lift $ updateLayout thisRoot 0 0 size
  modify $ \x -> setRoot x newRoot


oakMain :: (MonadFrontend m, MonadSurface m, Eq i, Show i)
           => OakT i m ()
oakMain = do
  lift $ initialize
  recalcLayout
  l <- gets root
  eventLoop


runOak :: (MonadFrontend m, MonadSurface m, Eq idt, Show idt) =>
          Widget idt -> m ()
runOak root = runOakT oakMain $ Display root Nothing
  
