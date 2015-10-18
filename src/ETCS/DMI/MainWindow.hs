{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeFamilies      #-}

module ETCS.DMI.MainWindow
       ( mkMainWindow
       , module ETCS.DMI.Window
       ) where

import           Control.Lens
import           ETCS.DMI.Helpers
import           ETCS.DMI.Types
import           ETCS.DMI.Widgets.Button
import           ETCS.DMI.Widgets.ButtonGroup
import           ETCS.DMI.Window
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
           , mkButton UpButton (pure $ pure "Train running Number") en
           , mkButton DelayButton (pure $ pure "Shunting") en
           , mkButton DelayButton (pure $ pure "Non-Leading") en
           , mkButton DelayButton (pure $ pure "Maintain Shunting") en
       ]
      return (MainWindow . fromWidgetInstance $  w, widgetRoot w)

instance IsEventWidget MainWindow where
  type WidgetEventType MainWindow = Either () Int
  widgetEvent = widgetEvent . mainWindow


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
                bAnd bLevel23 (fmap not $ i ^. trainHasPendingEmergencyStop)
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

