{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeFamilies      #-}

module ETCS.DMI.Windows.MainWindow
       ( mkMainWindow
       , module ETCS.DMI.Windows.Window
       ) where

import           Control.Lens
import           Data.Text                    (Text)
import           ETCS.DMI.Helpers
import           ETCS.DMI.TrainBehavior
import           ETCS.DMI.Types
import           ETCS.DMI.Widgets.Button
import           ETCS.DMI.Widgets.ButtonGroup
import           ETCS.DMI.Windows.Window
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.DOM.Widget


newtype MainWindow = MainWindow { mainWindow :: MenuWindow }


mkMainWindow :: TrainBehavior -> Behavior Bool -> Behavior Bool -> Behavior Bool ->
                WidgetInput MainWindow
mkMainWindow = MkMainWindow

instance IsWidget MainWindow where
  data WidgetInput MainWindow = MkMainWindow {
    _mainWindowTrainBehavior :: TrainBehavior,
    _mainWindowButtonsDisabled :: Behavior Bool,
    _mainWindowHourGlassVisible :: Behavior Bool,
    _mainWindowVisible :: Behavior Bool
  }

  mkWidgetInstance parent wi =
    let i = _mainWindowTrainBehavior wi
        titleIcon =
          (\g -> if g then pure "ST_05" else Nothing) <$>
            _mainWindowHourGlassVisible wi
        mainWinC = mkWindow (pure "Main") titleIcon (_mainWindowVisible wi)
        en = not <$> _mainWindowButtonsDisabled wi
    in do
      w <- mkSubWidget parent . mainWinC  . mkButtonGroup $
           [ mkButton UpButton (pure $ pure "Start")
             (i ^. startButtonEnabled en)
           , mkButton UpButton (pure $ pure "Driver ID")
             (i ^. driverIDButtonEnabled en)
           , mkButton UpButton (pure $ pure "Train Data")
             (i ^. trainDataButtonEnabled en)
           , mkEmptyButton
           , mkButton UpButton (pure $ pure "Level")
             (i ^. levelButtonEnabled en)
           , mkButton UpButton (pure $ pure "Train running Number")
             (i ^. runningNumberEnabled en)
           , mkButton DelayButton (i ^. shuntingButtonLabel)
             (i ^. shuntingButtonEnabled en)
           , mkButton DelayButton (pure $ pure "Non-Leading")
             (i ^. nonLeadingEnabled en)
           , mkButton DelayButton (pure $ pure "Maintain Shunting")
             (i ^. maintainShuntingEnabled en)
       ]
      return (MainWindow . fromWidgetInstance $  w, widgetRoot w)

instance IsEventWidget MainWindow where
  type WidgetEventType MainWindow = Either () Int
  widgetEvent = widgetEvent . mainWindow


maintainShuntingEnabled :: Behavior Bool -> Getter TrainBehavior (Behavior Bool)
maintainShuntingEnabled en = to $ \i ->
  bsAnd [en, i ^. trainInMode SH, i ^. trainPassiveShuntingInput ]

nonLeadingEnabled :: Behavior Bool -> Getter TrainBehavior (Behavior Bool)
nonLeadingEnabled en = to $ \i ->
  bsAnd [ en, i ^. trainIsAtStandstill, i ^. trainDriverIDIsValid
        , i ^. trainLevelIsValid
        , i ^. trainInModes [SB, SH, FS, LS, SR, OS]
        , i ^. trainNonLeadingInput
        ]

runningNumberEnabled :: Behavior Bool -> Getter TrainBehavior (Behavior Bool)
runningNumberEnabled en = to $ \i ->
  let rne1 = bsAnd [ i ^. trainIsAtStandstill, i ^. trainInMode SB
                   , i ^. trainDriverIDIsValid, i ^. trainLevelIsValid
                   ]
  in bAnd en $ bOr rne1 $ i ^. trainInModes [ FS, LS, SR, OS, NL, UN, SN ]


shuntingButtonLabel :: Getter TrainBehavior (Maybe (Behavior Text))
shuntingButtonLabel =
  to $ \i ->
  pure $ (\sh -> if sh then "Exit Shunting" else "Shunting") <$> i ^. trainInMode SH


shuntingButtonEnabled :: Behavior Bool -> Getter TrainBehavior (Behavior Bool)
shuntingButtonEnabled en = to $ \i ->
  let bShunting1 =
        bsAnd [ i ^. trainIsAtStandstill, i ^. trainDriverIDIsValid
              , i ^. trainInModes [SB, FS, LS, SR, OS, UN, SN]
              , i ^. trainLevelIsValid
              , (i ^. trainInLevels [Level0, Level1, NTC]) `bOr`
                ((i ^. trainInLevels [ Level2, Level3 ]) `bAnd`
                 (i ^. trainHasCommunicationSession)
                )
              ]
      bShunting2 =
        bsAnd [ i ^. trainIsAtStandstill, i ^. trainInMode PT
              , ( (i ^. trainInLevel Level1) `bOr`
                  ((i ^. trainInLevels [ Level2, Level3 ]) `bAnd`
                   (i ^. trainHasCommunicationSession) `bAnd`
                   (fmap not $ i ^. trainEmergencyStop)
                  )
                )
              ]
      bExitShunting =
        (i ^. trainIsAtStandstill) `bAnd` (i ^. trainInMode SH)
  in en `bAnd` (bShunting1 `bOr` bShunting2 `bOr` bExitShunting)


startButtonEnabled :: Behavior Bool -> Getter TrainBehavior (Behavior Bool)
startButtonEnabled en = to $ \i ->
  let bStartEnabled1 =
        bsAnd [ i ^. trainIsAtStandstill, i ^. trainInMode SB
              , i ^. trainDriverIDIsValid, i ^. trainDataIsValid
              , i ^. trainLevelIsValid, i ^. trainRunningNumberIsValid
              ]
      bStartEnabled2 =
        bsAnd [ i ^. trainIsAtStandstill, i ^. trainInMode PT
              , i ^. trainDataIsValid
              , bOr bLevel1 $
                bAnd bLevel23 (fmap not $ i ^. trainEmergencyStop)
              ]
      bStartEnabled3 = bAnd bLevel23 $ i ^. trainInMode SR
      bLevel1  = i ^. trainInLevel Level1
      bLevel23 = i ^. trainInLevels [Level2, Level3]
  in en `bAnd` ((bStartEnabled1 `bOr` bStartEnabled2) `bOr` bStartEnabled3)


driverIDButtonEnabled :: Behavior Bool -> Getter TrainBehavior (Behavior Bool)
driverIDButtonEnabled en = to $ \i ->
  let bDriverIdEnabled1 = bsAnd
        [ i ^. trainIsAtStandstill, i  ^. trainInMode SB
        , i ^. trainDriverIDIsValid, i ^. trainLevelIsValid
        ]
      bDriverIdEnabled2 = bOr (i ^. trainModDriverIDAllowed) $
        bsAnd [ fmap not $ i ^. trainModDriverIDAllowed
              , i ^. trainIsAtStandstill
              , i ^. trainInModes [ SH, FS, LS, SR, OS, NL, UN, SN ]
              ]
  in en `bAnd` (bDriverIdEnabled1 `bOr` bDriverIdEnabled2)


trainDataButtonEnabled :: Behavior Bool -> Getter TrainBehavior (Behavior Bool)
trainDataButtonEnabled en = to $ \i -> bsAnd
        [ en
        , i ^. trainIsAtStandstill, i ^. trainLevelIsValid
        , i ^. trainDriverIDIsValid, i ^. trainInModes [SB, FS, LS, SR, OS, UN, SN]
        ]

levelButtonEnabled :: Behavior Bool -> Getter TrainBehavior (Behavior Bool)
levelButtonEnabled en = to $ \i -> bsAnd
        [ en
        , i ^. trainIsAtStandstill
        , i ^. trainDriverIDIsValid, i ^. trainInModes [SB, FS, LS, SR, OS, NL, UN, SN]
        ]

