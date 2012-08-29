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

       , call
       , msgBox
       , inputBox

       , runOak
       )where

import Data.List (concatMap, find)
import Data.Maybe (isJust, fromMaybe)
import Data.Mutators
import Control.Applicative ((<$>))
import Control.Monad (forM)
import Control.Monad.RWS.Strict
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

newtype OakT i w m a =
  OakT (RWST [Handler i w m] (Last w) (OakState i m) m a)
  deriving ( Monad
           , MonadIO
           , MonadReader [Handler i w m]
           , MonadWriter (Last w)
           , MonadState (OakState i m)
           )

type Handler i w m = (i, Event, OakT i w m ())

bindHandlers :: i -> [(Event, OakT i w m ())] -> [Handler i w m]
bindHandlers i hs = map (\(e, h) -> (i, e, h)) hs


instance MonadTrans (OakT i w) where
  lift a = OakT (lift a)

instance (MonadFrontend m, MonadSurface m, Eq i, Show i) =>
         MonadHandler i w m (OakT i w m) where
  hlift  = hliftOak
  now    = nowOak
  alter  = alterOak
  open   = openOak
  answer = answerOak
  back   = backOak
  quit   = quitOak

hliftOak :: (MonadFrontend m, MonadSurface m) =>
            m a -> OakT i w m a
hliftOak act = lift act >>= return


nowOak :: (MonadFrontend m, MonadSurface m, Eq i) =>
          OakT i w m Integer
nowOak = liftIO currentSeconds


withUpdate :: (MonadFrontend m, MonadSurface m, Eq i) =>
              OakT i w m a -> OakT i w m ()
withUpdate act = act >> recalcLayout >> fixFocus


alterOak :: (MonadFrontend m, MonadSurface m, Eq i, Show i) =>
            i -> (Widget i m -> Widget i m) -> OakT i w m ()
alterOak i f = withUpdate $
               modify $ \s ->
               modDisplay s $ \d ->
               modRoot d $ updateInTree i f


openOak :: (MonadFrontend m, MonadSurface m, Eq i) =>
           Widget i m -> OakT i w m ()
openOak w = withUpdate $ do
  thisDisp <- gets display
  modify $ \s -> modLayers  s $ push thisDisp
  modify $ \s -> setDisplay s $ Display w Nothing


call :: (MonadFrontend m, MonadSurface m, Eq i, Show i) =>
          Widget i m -> [Handler i w m] -> m (Maybe w)
call wgt hs = runOakT (withUpdate (return ()) >> eventLoop) st hs
  where st = OakState { display = Display wgt Nothing
                      , layers  = stack
                      , running = True
                      }


answerOak :: (MonadFrontend m, MonadSurface m, Eq i, Show i) =>
             w -> OakT i w m ()
answerOak w = tell (Last $ Just w) >> quit

backOak :: (MonadFrontend m, MonadSurface m, Eq i, Show i) =>
           OakT i w m ()
backOak = withUpdate $ do
    st <- gets layers
    let (md, st') = pop st
    modify $ \s -> setLayers s st'
    maybe (return ()) (\d -> modify $ \s -> setDisplay s d) md


quitOak :: (Monad m, MonadIO m) => OakT i w m ()
quitOak = modify $ \s -> setRunning s False


runOakT :: MonadFrontend m
           => OakT i w m a
           -> OakState i m
           -> [Handler i w m]
           -> m (Maybe w)
runOakT (OakT oak) s hs = let p = evalRWST oak hs s
                          in liftM (getLast . snd) p


renderBox :: (MonadFrontend m, MonadSurface m, Eq i) =>
             [LayoutItem i m] -> OakT i w m ()
renderBox is = do
    f <- gets (focused . display)
    forM_ is $ \(LayoutItem i w r) -> case w of
        (Compact cw) -> render' cw Normal      r
        otherwise    -> render' w  (stFor i f) r
  where stFor i (Just f) = if i == f then Focused else Normal
        stFor _ Nothing  = Normal


render' :: (MonadFrontend m, MonadSurface m, Eq i) =>
           Widget i m -> WidgetState -> Rect -> OakT i w m ()
render' w st rc = case w of
  (VBox items) -> renderBox items
  (HBox items) -> renderBox items
  (Custom bh)  -> lift $ renderFcn bh st rc
  otherwise -> lift $ render w st rc


repaint :: (MonadFrontend m, MonadSurface m, Eq i)
           => OakT i w m ()
repaint = do
  wgt <- gets (root . display)
  sz <- lift $ surfSize
  render' wgt Normal $ Rect 0 0 sz


moveFocus :: (MonadFrontend m, Eq i) =>
             (Maybe i -> Widget i m -> Maybe i) -> OakT i w m ()
moveFocus f = do
  d <- gets display
  setFocus $  f (focused d) (root d)


standardKeyHandler :: (MonadFrontend m, MonadSurface m, Show i, Eq i) =>
                   i -> Key -> Widget i m -> OakT i w m ()
standardKeyHandler i k w = case w of
    (Button _) -> hkButton i k
    (Edit s c) -> hkEdit i k s c
    otherwise  -> return ()
  
hkButton :: (MonadFrontend m, Eq i) => i -> Key -> OakT i w m ()
hkButton i k
  | k `elem` [ArrowLeft,  ArrowUp]        = moveFocus prevFocus
  | k `elem` [ArrowRight, ArrowDown, Tab] = moveFocus nextFocus
  | otherwise = return ()


hkEdit :: (MonadFrontend m, MonadSurface m, Eq i, Show i) =>
          i -> Key -> String -> Int -> OakT i w m ()
hkEdit i k text caret = case k of
    ArrowLeft     -> alter i $ \_ -> Edit text (dec caret)
    ArrowRight    -> alter i $ \_ -> Edit text (inc caret)
    Home          -> alter i $ \_ -> Edit text 0
    End           -> alter i $ \_ -> Edit text $ length text

    Delete        -> alter i $ \_ -> Edit (del text caret) caret
    Backspace     -> alter i $ \_ -> Edit (bsp text caret) (dec caret)
    (Character c) -> alter i $ \_ -> Edit (ins text caret c) (succ caret)
    SpaceKey      -> alter i $ \_ -> Edit (ins text caret ' ') (succ caret)

    ArrowUp       -> moveFocus prevFocus
    ArrowDown     -> moveFocus nextFocus
    Tab           -> moveFocus nextFocus
    otherwise     -> return ()
  where inc c = let nc = succ c in if nc > l then c else nc
        dec c = let nc = pred c in if nc < 0 then 0 else nc
        l     = length text

        del t c = let (p, s) = splitAt c t
                  in p ++ if null s then [] else tail s

        bsp t c = let (p, s) = splitAt c t
                  in (if null p then [] else init p) ++ s

        ins t c ch = let (p, s) = splitAt c t
                     in p ++ ch:s

  
runHandler :: (MonadFrontend m, MonadSurface m) =>
              Handler i w m -> OakT i w m ()
runHandler (_, _, f) = f

handleKey :: (MonadFrontend m, MonadSurface m, Eq i, Show i) =>
             Key -> OakT i w m ()
handleKey k = focusedWidget >>= maybe (return ()) dispatch
  where dispatch (i, w) = do
          handler <- handlerFor i (KeyDown k)
          maybe (standardKeyHandler i k w) runHandler handler


handlerFor :: (Monad m, Eq i) =>
              i -> Event -> OakT i w m (Maybe (Handler i w m))
handlerFor i e = do handlers <- ask
                    return $ find match handlers
  where match (i', e', _) = i' == i && e' == e


handlersOn :: (Monad m, Eq i) =>
              Event -> OakT i w m [Handler i w m]
handlersOn e = do
  hs <- ask
  d  <- gets display
  return $ filter (onDisplay d) $ filter handles hs
  where handles (i, e', _)    = e' == e
        onDisplay d (i, w, _) = isJust $ lookupWidget i $ root d


processEvent :: (MonadFrontend m, MonadSurface m, Eq i, Show i) =>
                Event -> OakT i w m ()
processEvent e = case e of
  (KeyDown k) -> handleKey k
  Quit        -> quit
  otherwise   -> return ()


live :: (MonadFrontend m, MonadSurface m, Eq i) =>
        OakT i w m ()
live = handlersOn Live >>= mapM_ runHandler


eventLoop :: (MonadFrontend m, MonadSurface m, Eq i, Show i) =>
             OakT i w m ()
eventLoop = do
  running <- gets running
  if not running
    then return ()
    else do mapM processEvent =<< lift getEvents
            live >> repaint >> lift endIter
            eventLoop


focusedWidget :: (MonadFrontend m, Eq i) =>
                 OakT i w m (Maybe (i, Widget i m))
focusedWidget = do d <- gets display
                   return $ (focused d) @@ (root d)
  where mi @@ r = do i <- mi
                     w <- lookupWidget i r
                     return $ (i, w)


recalcLayout :: (MonadSurface m, Eq i) => OakT i w m ()
recalcLayout = do
  thisRoot <- gets (root . display)
  size <- lift $ surfSize
  newRoot <- lift $ updateLayout thisRoot 0 0 size
  modify $ \s -> modDisplay s $ \d -> setRoot d newRoot


setFocus :: MonadFrontend m => Maybe i -> OakT i w m ()
setFocus mi = modify $ \s -> modDisplay s $ \d -> setFocused d mi


fixFocus :: (MonadFrontend m, Eq i) => OakT i w m ()
fixFocus = do
    d <- gets display
    case (focused d) of
      (Just i) -> case lookupWidget i (root d) of
        (Just w) -> when (not $ acceptsFocus w) fallback
        otherwise -> fallback
      otherwise -> fallback 
  where fallback = do r <- gets (root . display)
                      setFocus $ findFirst acceptsFocus r


oakMain :: (MonadFrontend m, MonadSurface m, Eq i, Show i)
           => OakT i w m ()
oakMain = withUpdate (lift $ initialize) >> eventLoop


runOak :: (MonadFrontend m, MonadSurface m, Eq i, Show i) =>
          Widget i m -> [Handler i w m] -> m (Maybe w)
runOak wgt hs = runOakT oakMain st hs
  where st = OakState { display = Display wgt Nothing
                      , layers  = stack
                      , running = True
                      }


msgBox :: (MonadFrontend m, MonadSurface m) =>
          String -> String -> [MessageCode] -> m (Maybe MessageCode)
msgBox title text cs = call (messageBox title text cs) messageHandlers


inputBox :: (MonadFrontend m, MonadSurface m) =>
            String -> String -> m (Maybe String)
inputBox title text = call (inputDialog title text) inputBoxHandlers



data MsgId = BtnOk | BtnYes | BtnNo | BtnCancel
           | EdtEntry
           | Unused
             deriving (Eq, Show)

instance Identifier MsgId where
  unused  = Unused
  btnBack = BtnOk

msgTable = [ (Ok,     (BtnOk,     "Ok"))
           , (Yes,    (BtnYes,    "Yes"))
           , (No,     (BtnNo,     "No"))
           , (Cancel, (BtnCancel, "Cancel"))
           ]

idFor :: MessageCode -> MsgId
idFor c = fromMaybe BtnOk $ fst <$> lookup c msgTable

textFor :: MessageCode -> String
textFor c = fromMaybe "Ok" $ snd <$> lookup c msgTable
  
messageBox :: String -> String -> [MessageCode] -> Widget MsgId m
messageBox title text cs =
    dialog title btns (unused, center (unused, Label text))
  where btns = map (\c -> (idFor c, Button $ textFor c)) cs

messageHandlers :: (MonadFrontend m, MonadSurface m) =>
                   [(MsgId, Event, OakT MsgId MessageCode m ())]
messageHandlers = map mkHandler msgTable
   where mkHandler (c, (i, _)) = (i, KeyDown Return, answer c)


inputDialog :: String -> String -> Widget MsgId m
inputDialog title text = dialog title btns (Unused, contents)
  where contents = center (Unused, Compact $
                           vbox [ (Unused,   Label text)
                                , (EdtEntry, Edit "" 0)
                                ]
                          )
        btns = map (\c -> (idFor c, Button $ textFor c)) cs
        cs = [Ok, Cancel]

inputBoxHandlers :: (MonadFrontend m, MonadSurface m) =>
                    [(MsgId, Event, OakT MsgId String m ())]
inputBoxHandlers =
  [ (BtnOk,     KeyDown Return, answer "")
  , (EdtEntry,  KeyDown Return, answer "")
  , (BtnCancel, KeyDown Return, quit)
  ]