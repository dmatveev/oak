{-# LANGUAGE TemplateHaskell,
             GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             FlexibleInstances,
             UndecidableInstances #-}

module Graphics.UI.Oak
       (
         Widget(..)
       , Size(..)
       , Font(..)
       , Handler
       , OakState

       , bindHandlers

       , initOak
       , iterOak

       , call
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


eventLoop :: (MonadSurface m, MonadFrontend u m,
              Identifier i, Eq i, Show i) =>
             (OakState i m, Maybe w) ->
             [Handler i u w m] ->
             u ->
             (m (OakState i m, Maybe w) -> u -> IO ((OakState i m, Maybe w), u)) ->
             IO (Maybe w)
eventLoop (s, w) hs u r = do
  ((s', w'), u') <- r (iterOak s hs) u
  if not $ running s'
    then return w'
    else eventLoop (s', w') hs u' r
  

type Handler i u w m = (i, Event, OakT i u w m ())


newtype OakT i u w m a =
  OakT (RWST [Handler i u w m] (Last w) (OakState i m) m a)
  deriving ( Monad
           , MonadIO
           , MonadReader [Handler i u w m]
           , MonadWriter (Last w)
           , MonadState (OakState i m)
           )


bindHandlers :: i -> [(Event, OakT i u w m ())] -> [Handler i u w m]
bindHandlers i hs = map (\(e, h) -> (i, e, h)) hs


instance MonadTrans (OakT i u w) where
  lift a = OakT (lift a)

instance (MonadFrontend u m, MonadSurface m,
          Identifier i, Eq i, Show i) =>
         MonadHandler i w m (OakT i u w m) where
  hlift  = hliftOak
  now    = nowOak
  alter  = alterOak
  open   = openOak
  answer = answerOak
  back   = backOak
  quit   = quitOak

  msgBox = msgBoxOak
  inputBox = inputBoxOak


hliftOak :: (MonadFrontend u m, MonadSurface m) =>
            m a -> OakT i u w m a
hliftOak act = lift act >>= return


nowOak :: (MonadFrontend u m, MonadSurface m, Eq i) =>
          OakT i u w m Integer
nowOak = liftIO currentSeconds


withUpdate :: (MonadFrontend u m, MonadSurface m, Eq i) =>
              OakT i u w m a -> OakT i u w m ()
withUpdate act = act >> recalcLayout >> fixFocus


alterOak :: (MonadFrontend u m, MonadSurface m, Eq i, Show i) =>
            i -> (Widget i m -> Widget i m) -> OakT i u w m ()
alterOak i f = withUpdate $
               modify $ \s ->
               modDisplay s $ \d ->
               modRoot d $ updateInTree i f


openOak :: (MonadFrontend u m, MonadSurface m, Eq i) =>
           Widget i m -> OakT i u w m ()
openOak w = withUpdate $ do
  thisDisp <- gets display
  modify $ \s -> modLayers  s $ push thisDisp
  modify $ \s -> setDisplay s $ Display w Nothing


oakCont :: (MonadFrontend u m, MonadSurface m,
            Identifier i, Eq i, Show i)
           => OakT i u w m ()
oakCont = do
  withUpdate $ return ()
  st <- get
  hs <- ask
  u <- lift ownData
  r <- lift runFcn
  (_, w) <- listen (return ())
  mnw <- liftIO $ eventLoop (st, getLast w) hs u r
  tell $ Last mnw
  return ()
 

callOak :: (MonadFrontend u m, MonadSurface m, Eq i, Show i,
            Identifier oi, Eq oi, Show oi)
           => Widget oi m
           -> [Handler oi u ow m]
           -> OakT i u w m (Maybe ow)
callOak w hs = do
    (_, w2) <- lift $ runOakT oakCont st hs
    return w2
  where st = OakState { display = Display w Nothing
                      , layers  = stack
                      , running = True
                      }


-- call a b = undefined
-- call :: (MonadFrontend u m, MonadSurface m, Eq i, Show i) =>
--           Widget i m -> [Handler i u w m] -> m (Maybe w)
-- call _ _ = undefined
-- -- call wgt hs = liftM snd $ runOakT (withUpdate (return ()) >> eventLoop) st hs
-- --   where st = OakState { display = Display wgt Nothing
-- --                       , layers  = stack
-- --                       , running = True
-- --                       }


answerOak :: (MonadFrontend u m, MonadSurface m,
              Identifier i, Eq i, Show i) =>
             w -> OakT i u w m ()
answerOak w = tell (Last $ Just w) >> quit

backOak :: (MonadFrontend u m, MonadSurface m,
            Identifier i, Eq i, Show i) =>
           OakT i u w m ()
backOak = withUpdate $ do
    st <- gets layers
    let (md, st') = pop st
    modify $ \s -> setLayers s st'
    maybe (return ()) (\d -> modify $ \s -> setDisplay s d) md


quitOak :: (Monad m, MonadIO m) => OakT i u w m ()
quitOak = modify $ \s -> setRunning s False


runOakT :: MonadFrontend u m
           => OakT i u w m a
           -> OakState i m
           -> [Handler i u w m]
           -> m (OakState i m, Maybe w)
runOakT (OakT oak) s e = do
  (st, lw) <- execRWST oak e s
  return (st, getLast lw)


renderBox :: (MonadFrontend u m, MonadSurface m, Eq i) =>
             [LayoutItem i m] -> OakT i u w m ()
renderBox is = do
    f <- gets (focused . display)
    forM_ is $ \(LayoutItem i w r) -> render' w  (stFor i f) r
  where stFor i (Just f) = if i == f then Focused else Normal
        stFor _ Nothing  = Normal


render' :: (MonadFrontend u m, MonadSurface m, Eq i) =>
           Widget i m -> WidgetState -> Rect -> OakT i u w m ()
render' w st rc@(Rect x y sz) = case w of
  (VBox items) -> renderBox items
  (HBox items) -> renderBox items
  (Margin (l, t, r, b) w) ->
    render' w st $ Rect (x + l) (y + t) (decrease sz (l + r) (t + b))
  (Compact cw) -> render' cw Normal rc
  (Custom bh)  -> lift $ renderFcn bh st rc
  otherwise -> lift $ render w st rc


repaint :: (MonadFrontend u m, MonadSurface m, Eq i)
           => OakT i u w m ()
repaint = do
  wgt <- gets (root . display)
  sz <- lift $ surfSize
  render' wgt Normal $ Rect 0 0 sz


moveFocus :: (MonadFrontend u m, Eq i) =>
             (Maybe i -> Widget i m -> Maybe i) -> OakT i u w m ()
moveFocus f = do
  d <- gets display
  setFocus $  f (focused d) (root d)


standardKeyHandler :: (MonadFrontend u m, MonadSurface m,
                       Identifier i, Show i, Eq i) =>
                   i -> Key -> Widget i m -> OakT i u w m ()
standardKeyHandler i k w = case w of
    (Button _) -> hkButton i k
    (Edit s c) -> hkEdit i k s c
    otherwise  -> return ()
  
hkButton :: (MonadFrontend u m, Eq i) => i -> Key -> OakT i u w m ()
hkButton i k
  | k `elem` [ArrowLeft,  ArrowUp]        = moveFocus prevFocus
  | k `elem` [ArrowRight, ArrowDown, Tab] = moveFocus nextFocus
  | otherwise = return ()


hkEdit :: (MonadFrontend u m, MonadSurface m,
           Identifier i, Eq i, Show i) =>
          i -> Key -> String -> Int -> OakT i u w m ()
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

  
runHandler :: (MonadFrontend u m, MonadSurface m) =>
              Handler i u w m -> OakT i u w m ()
runHandler (_, _, f) = f

handleKey :: (MonadFrontend u m, MonadSurface m,
              Identifier i, Eq i, Show i) =>
             Key -> OakT i u w m ()
handleKey k = focusedWidget >>= maybe (return ()) dispatch
  where dispatch (i, w) = do
          handler <- handlerFor i (KeyDown k)
          maybe (standardKeyHandler i k w) runHandler handler


handlerFor :: (Monad m, Eq i) =>
              i -> Event -> OakT i u w m (Maybe (Handler i u w m))
handlerFor i e = do handlers <- ask
                    return $ find match handlers
  where match (i', e', _) = i' == i && e' == e


handlersOn :: (Monad m, Eq i) =>
              Event -> OakT i u w m [Handler i u w m]
handlersOn e = do
  hs <- ask
  d  <- gets display
  return $ filter (onDisplay d) $ filter handles hs
  where handles (i, e', _)    = e' == e
        onDisplay d (i, w, _) = isJust $ lookupWidget i $ root d


processEvent :: (MonadFrontend u m, MonadSurface m,
                 Identifier i, Eq i, Show i) =>
                Event -> OakT i u w m ()
processEvent e = case e of
  (KeyDown k) -> handleKey k
  Quit        -> quit
  otherwise   -> return ()


live :: (MonadFrontend u m, MonadSurface m, Eq i) =>
        OakT i u w m ()
live = handlersOn Live >>= mapM_ runHandler


-- eventLoop :: (MonadFrontend u m, MonadSurface m, Eq i, Show i) =>
--              OakT i u w m ()
-- eventLoop = do
--   running <- gets running
--   if not running
--     then return ()
--     else do mapM processEvent =<< lift getEvents
--             live >> repaint >> lift endIter
--             eventLoop

eventLoopIteration :: (MonadFrontend u m, MonadSurface m,
                       Identifier i, Eq i, Show i) =>
                      OakT i u w m ()
eventLoopIteration = do mapM processEvent =<< lift getEvents
                        live >> repaint >> lift endIter


focusedWidget :: (MonadFrontend u m, Eq i) =>
                 OakT i u w m (Maybe (i, Widget i m))
focusedWidget = do d <- gets display
                   return $ (focused d) @@ (root d)
  where mi @@ r = do i <- mi
                     w <- lookupWidget i r
                     return $ (i, w)


recalcLayout :: (MonadSurface m, Eq i) => OakT i u w m ()
recalcLayout = do
  thisRoot <- gets (root . display)
  size <- lift $ surfSize
  newRoot <- lift $ updateLayout thisRoot 0 0 size
  modify $ \s -> modDisplay s $ \d -> setRoot d newRoot


setFocus :: MonadFrontend u m => Maybe i -> OakT i u w m ()
setFocus mi = modify $ \s -> modDisplay s $ \d -> setFocused d mi


fixFocus :: (MonadFrontend u m, Eq i) => OakT i u w m ()
fixFocus = do
    d <- gets display
    case (focused d) of
      (Just i) -> case lookupWidget i (root d) of
        (Just w) -> when (not $ acceptsFocus w) fallback
        otherwise -> fallback
      otherwise -> fallback 
  where fallback = do r <- gets (root . display)
                      setFocus $ findFirst acceptsFocus r


oakMain :: (MonadFrontend u m, MonadSurface m,
            Identifier i, Eq i, Show i)
           => OakT i u w m ()
oakMain = withUpdate (lift initialize) >> oakCont
  
 

initOak :: (MonadFrontend u m, MonadSurface m,
            Identifier i, Eq i, Show i)
           => Widget i m
           -> [Handler i u w m]
           -> m (OakState i m, Maybe w)
initOak wgt hs = runOakT oakMain st hs
  where st = OakState { display = Display wgt Nothing
                      , layers  = stack
                      , running = True
                      }

iterOak :: (MonadFrontend u m, MonadSurface m,
            Identifier i, Eq i, Show i)
           => OakState i m
           -> [Handler i u w m]
           -> m (OakState i m, Maybe w)
iterOak st hs = runOakT eventLoopIteration st hs


data MsgId = BtnOk | BtnYes | BtnNo | BtnCancel | EdtEntry
           | Unused
             deriving (Eq, Show)

instance Identifier MsgId where
  unused = Unused
  btnBack = BtnOk


msgBoxOak :: (MonadFrontend u m, MonadSurface m,
              Identifier i, Eq i, Show i)
          => String
          -> String
          -> [MessageCode]
          -> OakT i u w m (Maybe MessageCode)
msgBoxOak title text cs = callOak wgt messageHandlers
  where wgt = messageBox title text cs


inputBoxOak :: (MonadFrontend u m, MonadSurface m,
                Identifier i, Eq i, Show i) =>
            String -> String -> OakT i u w m (Maybe String)
inputBoxOak title text = callOak dlg inputBoxHandlers
  where dlg = inputDialog title text


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

messageHandlers :: (MonadFrontend u m, MonadSurface m) =>
                   [(MsgId, Event, OakT MsgId u MessageCode m ())]
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

inputBoxHandlers :: (MonadFrontend u m, MonadSurface m) =>
                    [(MsgId, Event, OakT MsgId u String m ())]
inputBoxHandlers =
  [ (BtnOk,     KeyDown Return, returnEnteredText)
  , (EdtEntry,  KeyDown Return, returnEnteredText)
  , (BtnCancel, KeyDown Return, quit)
  ]

returnEnteredText :: (MonadFrontend u m, MonadSurface m) =>
                     OakT MsgId u String m ()
returnEnteredText = do
    r <- gets (root . display)
    maybe quit process $ lookupWidget EdtEntry r
  where process (Edit s _ ) = answer s
        process _           = quit


call :: (MonadFrontend u m, MonadSurface m,
            Identifier i, Eq i, Show i)
           => Widget i m
           -> [Handler i u w m]
           -> m (Maybe w)
call w hs = do
    (_, w2) <- runOakT oakCont st hs
    return w2
  where st = OakState { display = Display w Nothing
                      , layers  = stack
                      , running = True
                      }
