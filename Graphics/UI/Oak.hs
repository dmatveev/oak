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

       , bindHandlers

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

data Display i m = Display {
    root    :: Widget i m
  , focused :: Maybe i
  } deriving (Eq, Show)

data OakState i m = OakState {
    display :: Display i m
  , layers  :: Stack (Display i m)
  , running :: Bool
  } deriving (Eq, Show)

genMutators ''Display
genMutators ''OakState

newtype OakT i m a = OakT (RWST [Handler i m] () (OakState i m) m a)
                     deriving ( Monad
                              , MonadIO
                              , MonadReader [Handler i m]
                              , MonadState (OakState i m)
                              )

type Handler i m = (i, Event, OakT i m ())

bindHandlers :: i -> [(Event, OakT i m ())] -> [Handler i m]
bindHandlers i hs = map (\(e, h) -> (i, e, h)) hs


instance MonadTrans (OakT i) where
  lift a = OakT (lift a)

instance (MonadFrontend m, MonadSurface m, Eq i) =>
         MonadHandler i m (OakT i m) where
  alter = alterOak
  open = openOak
  back = backOak
  quit = quitOak
  hlift = liftOak

withUpdate :: (MonadFrontend m, MonadSurface m, Eq i) =>
              OakT i m a -> OakT i m ()
withUpdate act = act >> recalcLayout >> fixFocus


alterOak :: (MonadFrontend m, MonadSurface m, Eq i) =>
            i -> (Widget i m -> Widget i m) -> OakT i m ()
alterOak i f = withUpdate $
               modify $ \s ->
               modDisplay s $ \d ->
               modRoot d $ updateInTree i f


openOak :: (MonadFrontend m, MonadSurface m, Eq i) =>
           Widget i m -> OakT i m ()
openOak w = withUpdate $ do
  thisDisp <- gets display
  modify $ \s -> modLayers  s $ push thisDisp
  modify $ \s -> setDisplay s $ Display w Nothing


backOak :: (MonadFrontend m, MonadSurface m, Eq i) => OakT i m ()
backOak = withUpdate $ do
    st <- gets layers
    let (md, st') = pop st
    modify $ \s -> setLayers s st'
    maybe (return ()) (\d -> modify $ \s -> setDisplay s d) md


quitOak :: (Monad m, MonadIO m) => OakT i m ()
quitOak = modify $ \s -> setRunning s False

liftOak :: (MonadFrontend m, MonadSurface m) => m a -> OakT i m a
liftOak act = lift act >>= return

runOakT :: MonadFrontend m =>
           OakT i m a -> OakState i m -> [Handler i m] -> m a
runOakT (OakT oak) s hs = let p = evalRWST oak hs s
                          in liftM fst p


renderBox :: (MonadFrontend m, MonadSurface m, Eq i) =>
             [LayoutItem i m] -> OakT i m ()
renderBox is = do
    f <- gets (focused . display)
    forM_ is $ \(LayoutItem i w r) -> case w of
        (Compact cw) -> render' cw Normal      r
        otherwise    -> render' w  (stFor i f) r
  where stFor i (Just f) = if i == f then Focused else Normal
        stFor _ Nothing  = Normal


render' :: (MonadFrontend m, MonadSurface m, Eq i) =>
           Widget i m -> WidgetState -> Rect -> OakT i m ()
render' w st rc = case w of
  (VBox items) -> renderBox items
  (HBox items) -> renderBox items
  (Custom bh)  -> lift $ renderFcn bh st rc
  otherwise -> lift $ render w st rc


repaint :: (MonadFrontend m, MonadSurface m, Eq i)
           => OakT i m ()
repaint = do
  wgt <- gets (root . display)
  sz <- lift $ surfSize
  render' wgt Normal $ Rect 0 0 sz


moveFocus :: (MonadFrontend m, Eq i) =>
             HandleResult -> OakT i m ()
moveFocus hr = do
  d <- gets display
  setFocus $ processFocus hr (focused d) (root d)


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


handlerFor :: (Monad m, Eq i) =>
              i -> Event -> OakT i m (Maybe (Handler i m))
handlerFor i e = do handlers <- ask
                    return $ find match handlers
  where match (i', e', _) = i' == i && e' == e


processEvent :: (MonadFrontend m, MonadSurface m, Eq i) =>
                Event -> OakT i m ()
processEvent e = case e of
  (KeyDown k) -> handleKey k
  Quit        -> quit
  otherwise   -> return ()


live :: (MonadFrontend m, MonadSurface m, Eq i) =>
        OakT i m ()
live = do
    now  <- liftIO $ currentSeconds
    disp <- gets display
    runLive now $ root disp
  where runLive t (VBox is)   = mapM_ (runLive t) $ map widget is
        runLive t (HBox is)   = mapM_ (runLive t) $ map widget is
        runLive t (Compact w) = runLive t w
        runLive t (Custom bh) = lift $ liveFcn bh t
        runLive _ _           = return ()


eventLoop :: (MonadFrontend m, MonadSurface m, Eq i) =>
             OakT i m ()
eventLoop = do
  running <- gets running
  if not running
    then return ()
    else do mapM processEvent =<< lift getEvents
            live >> repaint >> lift endIter
            eventLoop


focusedWidget :: (MonadFrontend m, Eq i) =>
                 OakT i m (Maybe (i, Widget i m))
focusedWidget = do d <- gets display
                   return $ (focused d) @@ (root d)
  where mi @@ r = do i <- mi
                     w <- lookupWidget i r
                     return $ (i, w)


recalcLayout :: (MonadSurface m, Eq i) => OakT i m ()
recalcLayout = do
  thisRoot <- gets (root . display)
  size <- lift $ surfSize
  newRoot <- lift $ updateLayout thisRoot 0 0 size
  modify $ \s -> modDisplay s $ \d -> setRoot d newRoot


setFocus :: MonadFrontend m => Maybe i -> OakT i m ()
setFocus mi = modify $ \s -> modDisplay s $ \d -> setFocused d mi


fixFocus :: (MonadFrontend m, Eq i) => OakT i m ()
fixFocus = do
    d <- gets display
    case (focused d) of
      (Just i) -> case lookupWidget i (root d) of
        (Just w) -> when (not $ acceptsFocus w) fallback
        otherwise -> fallback
      otherwise -> fallback 
  where fallback = do r <- gets (root . display)
                      setFocus $ findFirst acceptsFocus r


oakMain :: (MonadFrontend m, MonadSurface m, Eq i)
           => OakT i m ()
oakMain = withUpdate (lift $ initialize) >> eventLoop


runOak :: (MonadFrontend m, MonadSurface m, Eq i) =>
          Widget i m -> [Handler i m] -> m ()
runOak wgt hs = runOakT oakMain st hs
  where st = OakState { display = Display wgt Nothing
                      , layers  = stack
                      , running = True
                      }
