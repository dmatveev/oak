{-# LANGUAGE FlexibleContexts #-}

module Graphics.UI.Oak.Internal.Dialogs
       (
         messageBox
       , messageBoxHandlers
       , inputDialog
       , inputDialogHandlers
       ) where


import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)

import Graphics.UI.Oak.Basics
import Graphics.UI.Oak.Widgets
import Graphics.UI.Oak.Classes

data MsgId = BtnOk | BtnYes | BtnNo | BtnCancel | EdtEntry
           | Unused
             deriving (Eq, Show)

instance Identifier MsgId where
  unused = Unused
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


messageBoxHandlers :: MonadHandler MsgId MessageCode u m =>
                      [(MsgId, Event, m ())]
messageBoxHandlers = map mkHandler msgTable
   where mkHandler (c, (i, _)) = (i, KeyDown Return, answer c)


inputDialog :: String -> String -> Widget MsgId m
inputDialog title text = dialog title btns (Unused, contents)
  where contents = center (Unused, compact $
                           vbox [ (Unused,   Label text)
                                , (EdtEntry, Edit "" 0)
                                ]
                          )
        btns = map (\c -> (idFor c, Button $ textFor c)) cs
        cs = [Ok, Cancel]


inputDialogHandlers :: MonadHandler MsgId String u m =>
                       [(MsgId, Event, m ())]
inputDialogHandlers =
  [ (BtnOk,     KeyDown Return, returnEnteredText)
  , (EdtEntry,  KeyDown Return, returnEnteredText)
  , (BtnCancel, KeyDown Return, quit)
  ]

returnEnteredText :: MonadHandler MsgId String mh m => m ()
returnEnteredText = lWidget EdtEntry >>= maybe quit process
  where process (Edit s _ ) = answer s
        process _           = quit
