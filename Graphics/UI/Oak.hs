{-# LANGUAGE TemplateHaskell,
             GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             FlexibleInstances #-}

module Graphics.UI.Oak
       (
         Widget(..)
       , Size(..)
       , Font(..)
       , Handler

       , vbox
       , hbox

       , runOak
       )where

import Data.List (concatMap, find)
import Data.Mutators
import Control.Monad (forM)
import Control.Monad.RWS
import Control.Monad.Trans

import Graphics.UI.Oak.Basics
import Graphics.UI.Oak.Classes
import Graphics.UI.Oak.Internal.Focus
import Graphics.UI.Oak.Internal.Layout
import Graphics.UI.Oak.Internal.Tree
import Graphics.UI.Oak.Utils
import Graphics.UI.Oak.Widgets

data Display i = Display {
    root    :: Widget i
  , layers  :: Stack (Widget i)
  , running :: Bool
  , focused :: Maybe i
  } deriving (Eq, Show)

genMutators ''Display


newtype OakT i m a = OakT (RWST [Handler i m] () (Display i) m a)
                     deriving ( Monad
                              , MonadIO
                              , MonadReader [Handler i m]
                              , MonadState (Display i)
                              )

type Handler i m = (i, Event, OakT i m ())

instance MonadTrans (OakT i) where
  lift a = OakT (lift a)

instance (MonadFrontend m, MonadSurface m, Eq i) =>
         MonadHandler i (OakT i m) where
  alter = alterOak
  open = openOak
  back = backOak
  quit = quitOak

withUpdate :: (MonadFrontend m, MonadSurface m, Eq i) =>
              OakT i m a -> OakT i m ()
withUpdate act = act >> recalcLayout >> fixFocus


alterOak :: (MonadFrontend m, MonadSurface m, Eq i) =>
            i -> (Widget i -> Widget i) -> OakT i m ()
alterOak i f = withUpdate $ modify $ \s -> modRoot s $ updateInTree i f


openOak :: (MonadFrontend m, MonadSurface m, Eq i) =>
           Widget i -> OakT i m ()
openOak w = withUpdate $ do
  thisRoot <- gets root
  modify $ \s -> modLayers s $ push thisRoot
  modify $ \s -> setRoot s w


backOak :: (MonadFrontend m, MonadSurface m, Eq i) => OakT i m ()
backOak = withUpdate $ do
  st <- gets layers
  let (mw, st') = pop st
  modify $ \s -> setLayers s st'
  maybe (return ()) (\w -> modify $ \s -> setRoot s w) mw


quitOak :: (Monad m, MonadIO m) => OakT i m ()
quitOak = modify $ \s -> setRunning s False

runOakT :: MonadFrontend m =>
           OakT i m a -> Display i -> [Handler i m] -> m a
runOakT (OakT oak) d hs = let p = evalRWST oak hs d
                          in liftM fst p


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


moveFocus :: (MonadFrontend m, Eq i) =>
             HandleResult -> OakT i m ()
moveFocus hr = do
  cur <- gets focused
  wgt <- gets root
  setFocus $ processFocus hr cur wgt


type KeyHandler i m a = i -> Key -> OakT i m a

btnKeyHandler :: (MonadFrontend m, Eq i) => KeyHandler i m ()
btnKeyHandler i k
  | k `elem` [ArrowLeft, ArrowUp]    = moveFocus PrevFocus
  | k `elem` [ArrowRight, ArrowDown] = moveFocus NextFocus
  | otherwise = return ()


runHandler :: MonadFrontend m => Handler i m -> OakT i m ()
runHandler (_, _, f) = f

handleKey :: (MonadFrontend m, Eq i) => Key -> OakT i m ()
handleKey k = focusedWidget >>= maybe (return ()) dispatch
  where dispatch (i, w) = do
          handler <- handlerFor i (KeyDown k)
          maybe (btnKeyHandler i k) runHandler handler


processEvent :: (MonadFrontend m, Eq i) => Event -> OakT i m ()
processEvent e = case e of
  (KeyDown k) -> handleKey k
  otherwise   -> return ()


handlerFor :: (Monad m, Eq i) =>
              i -> Event -> OakT i m (Maybe (Handler i m))
handlerFor i e = do handlers <- ask
                    return $ find match handlers
  where match (i', e', _) = i' == i && e' == e


eventLoop :: (MonadFrontend m, MonadSurface m, Eq i) =>
             OakT i m ()
eventLoop = do
  running <- gets running
  if not running
    then return ()
    else do events <- lift $ getEvents
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


recalcLayout :: (MonadSurface m, Eq i) => OakT i m ()
recalcLayout = do
  thisRoot <- gets root
  size <- lift $ surfSize
  newRoot <- lift $ updateLayout thisRoot 0 0 size
  modify $ \x -> setRoot x newRoot


setFocus :: MonadFrontend m => Maybe i -> OakT i m ()
setFocus mi = modify $ \s -> setFocused s mi


fixFocus :: (MonadFrontend m, Eq i) => OakT i m ()
fixFocus = do
    r <- gets root
    f <- gets focused
    case f of
      (Just i) -> case (lookupWidget i r) of
        (Just w) -> when (not $ acceptsFocus w) fallback
        otherwise -> fallback
      otherwise -> fallback 
  where fallback = gets root >>= (setFocus . findFirst acceptsFocus)


oakMain :: (MonadFrontend m, MonadSurface m, Eq i)
           => OakT i m ()
oakMain = withUpdate (lift $ initialize) >> eventLoop


runOak :: (MonadFrontend m, MonadSurface m, Eq i) =>
          Widget i -> [Handler i m] -> m ()
runOak wgt hs = runOakT oakMain disp hs
  where disp = Display { root    = wgt
                       , focused = Nothing
                       , layers  = stack
                       , running = True
                       }
