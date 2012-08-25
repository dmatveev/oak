{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Graphics.UI.Oak
       (
         Widget(..)
       , Size(..)
       , Font(..)

       , vbox
       , hbox

       , runOak
       )where

import Data.List (concatMap)
import Data.Mutators
import Control.Monad (forM)
import Control.Monad.State
import Control.Monad.Trans

import Graphics.UI.Oak.Basics
import Graphics.UI.Oak.Classes
import Graphics.UI.Oak.Internal.Focus
import Graphics.UI.Oak.Internal.Layout
import Graphics.UI.Oak.Internal.Tree
import Graphics.UI.Oak.Utils
import Graphics.UI.Oak.Widgets

data Display idt = Display {
    root    :: Widget idt
  , focused :: Maybe idt
  } deriving (Eq, Show)

genMutators ''Display


newtype OakT i m a = OakT (StateT (Display i) m a)
                     deriving ( Monad
                              , MonadIO
                              , MonadTrans
                              , MonadState (Display i)
                              )

runOakT :: MonadFrontend m => OakT i m a -> Display i -> m a
runOakT (OakT stt) d = evalStateT stt d


renderBox :: (MonadFrontend m, MonadSurface m, Eq i) =>
             [LayoutItem i] -> OakT i m ()
renderBox is = do
    f <- gets focused
    forM_ is $ \(LayoutItem i w r) -> render' w (stFor i f) r
  where stFor i (Just f) = if i == f then Focused else Normal
        stFor _ Nothing  = Normal


render' :: (MonadFrontend m, MonadSurface m, Eq i) =>
           Widget i -> WidgetState -> Rect -> OakT i m ()
render' w st rc = case w of
  (VBox items) -> renderBox items
  (HBox items) -> renderBox items
  otherwise -> lift $ render w st rc


repaint :: (MonadFrontend m, MonadSurface m, Eq i) => OakT i m ()
repaint = do
  wgt <- gets root
  sz <- lift $ surfSize
  render' wgt Normal $ Rect 0 0 sz


type KeyHandler i m a = i -> Key -> OakT i m a

btnKeyHandler :: (MonadFrontend m) => KeyHandler i m HandleResult
btnKeyHandler i k = return $
  if k `elem` [ArrowLeft, ArrowUp]
  then PrevFocus
  else if (k `elem` [ArrowRight, ArrowDown])
         then NextFocus
         else NoResult      


handleKey :: (MonadFrontend m, Eq i) => Key -> OakT i m ()
handleKey k = focusedWidget >>= maybe (return ()) dispatch
  where dispatch (i, (Button s)) = do
          res <- btnKeyHandler i k
          cur <- gets focused
          wgt <- gets root
          modify $ \s -> setFocused s $ processFocus res cur wgt


processEvent :: (MonadFrontend m, Eq i) => Event -> OakT i m ()
processEvent e = case e of
  (KeyDown k) -> handleKey k
  otherwise   -> return ()


eventLoop :: (MonadFrontend m, MonadSurface m, Eq i) =>
             OakT i m ()
eventLoop = do
  events <- lift $ getEvents
  if any (== Quit) events
    then return ()
    else do forM_ events processEvent
            repaint
            lift endIter
            eventLoop


focusedWidget :: (MonadFrontend m, Eq i) =>
                 OakT i m (Maybe (i, Widget i))
focusedWidget = do current <- gets focused
                   rootWgt <- gets root
                   return $ current @@ rootWgt
  where mi @@ r = do i <- mi
                     w <- lookupWidget i r
                     return $ (i, w)


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
  r <- gets root
  modify $ \s -> setFocused s $ findFirst acceptsFocus r
  eventLoop


runOak :: (MonadFrontend m, MonadSurface m, Eq i, Show i) =>
          Widget i -> m ()
runOak root = runOakT oakMain $ Display root Nothing
