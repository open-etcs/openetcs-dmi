{-# LANGUAGE OverloadedStrings #-}

module ETCS.DMI.MenuWindows
       ( mkMainWindow, mkOverrideWindow, mkSpecialWindow, mkSettingsWindow
       , mkRBCContactWindow
       , module ETCS.DMI.Window
       ) where

import           Control.Lens
import           ETCS.DMI.Button
import           ETCS.DMI.ButtonGroup
import           ETCS.DMI.Helpers
import           ETCS.DMI.Types
import           ETCS.DMI.Window
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.Frameworks

trainInMode :: ETCSMode -> TrainBehavior -> Behavior Bool
trainInMode m i = fmap (m ==) $ i ^. trainMode

trainInModes :: [ETCSMode] -> TrainBehavior -> Behavior Bool
trainInModes ms i = fmap (`elem` ms) $ i ^. trainMode




mkMainWindow :: (IsNode p) =>
                TrainBehavior -> p -> Behavior Bool -> MomentIO MenuWindow
mkMainWindow i parent visible =
  mkWidget parent $ mkWindow (pure "Main") visible . mkButtonGroup $
  [ mkButton UpButton (pure $ pure "Start") (bStartButtonEnabled i)
  , mkButton UpButton (pure $ pure "Driver ID") (bDriverIDButtonEnabled i)
  , mkButton UpButton (pure $ pure "Train Data") (bTrainDataButtonEnabled i)
  , mkButton UpButton Nothing (pure False)
  , mkButton UpButton (pure $ pure "Level") (bLevelButtonEnabled i)
  , mkButton UpButton (pure $ pure "Train running Number") (pure True)
  , mkButton DelayButton (pure $ pure "Shunting") (pure True)
  , mkButton DelayButton (pure $ pure "Non-Leading") (pure True)
  , mkButton DelayButton (pure $ pure "Maintain Shunting") (pure False)
  ]




bStartButtonEnabled :: TrainBehavior -> Behavior Bool
bStartButtonEnabled i =
  let bStartEnabled1 = bsAnd
        [ i ^. trainIsAtStandstill, trainInMode SB i
        , i ^. trainDriverIDIsValid, i ^. trainDataIsValid
        , i ^. trainLevelIsValid, i ^. trainRunningNumberIsValid
        ]
      bStartEnabled2 = bsAnd
        [ i ^. trainIsAtStandstill, trainInMode PT i
        , i ^. trainDataIsValid
        , bOr bLevel1 $ bAnd bLevel23 (fmap not $ i ^. trainHasPendingEmergencyStop)
        ]
      bStartEnabled3 = bAnd bLevel23 $ trainInMode SR i
      bLevel1  = fmap (Level1 ==) $ i ^. trainLevel
      bLevel23 = fmap (`elem` [Level2, Level3]) $ i ^. trainLevel
  in (bStartEnabled1 `bOr` bStartEnabled2) `bOr` bStartEnabled3



bDriverIDButtonEnabled :: TrainBehavior -> Behavior Bool
bDriverIDButtonEnabled i =
  let bDriverIdEnabled1 = bsAnd
        [ i ^. trainIsAtStandstill, trainInMode SB i
        , i ^. trainDriverIDIsValid, i ^. trainLevelIsValid
        ]
      bDriverIdEnabled2 = bOr (i ^. trainModDriverIDAllowed) $
        bsAnd [ fmap not $ i ^. trainModDriverIDAllowed
              , i ^. trainIsAtStandstill
              , trainInModes [ SH, FS, LS, SR, OS, NL, UN, SN ] i
              ]
  in bDriverIdEnabled1 `bOr` bDriverIdEnabled2


bTrainDataButtonEnabled :: TrainBehavior -> Behavior Bool
bTrainDataButtonEnabled i = bsAnd
        [ i ^. trainIsAtStandstill, i ^. trainLevelIsValid
        , i ^. trainDriverIDIsValid, trainInModes [SB, FS, LS, SR, OS, UN, SN] i
        ]

bLevelButtonEnabled :: TrainBehavior -> Behavior Bool
bLevelButtonEnabled i = bsAnd
        [ i ^. trainIsAtStandstill
        , i ^. trainDriverIDIsValid, trainInModes [SB, FS, LS, SR, OS, NL, UN, SN] i
        ]








mkOverrideWindow :: (IsNode p) => p -> Behavior Bool -> MomentIO MenuWindow
mkOverrideWindow parent visible =
  mkWidget parent $ mkWindow (pure "Override") visible . mkButtonGroup $
  [ mkButton UpButton (pure $ pure "EOA") (pure True)
  ]


mkSpecialWindow :: (IsNode p) => p -> Behavior Bool -> MomentIO MenuWindow
mkSpecialWindow parent visible =
  mkWidget parent $ mkWindow (pure "Special") visible . mkButtonGroup $
  [ mkButton UpButton (pure $ pure "Ahension") (pure True)
  , mkButton UpButton (pure $ pure "SR speed / distance") (pure True)
  , mkButton DelayButton (pure $ pure "Train integrety") (pure True)
  ]

mkSettingsWindow :: (IsNode p) => p -> Behavior Bool -> MomentIO MenuWindow
mkSettingsWindow parent visible =
  mkWidget parent $ mkWindow (pure "Settings") visible . mkButtonGroup $
  [ mkButton UpButton (pure $ pure "Language") (pure True) -- TODO: Image SE03
  , mkButton UpButton (pure $ pure "Volume") (pure True) -- TODO: Image SE02
  , mkButton UpButton (pure $ pure "Brightness") (pure True) -- TODO: Image SE01
  , mkButton UpButton (pure $ pure "System version") (pure True)
  , mkButton UpButton (pure $ pure "Set VBC") (pure True)
  , mkButton UpButton (pure $ pure "Remove VBC") (pure True)
  ]

mkRBCContactWindow :: (IsNode p) => p -> Behavior Bool -> MomentIO MenuWindow
mkRBCContactWindow parent visible =
  mkWidget parent $ mkWindow (pure "RBC Contact") visible . mkButtonGroup $
  [ mkButton UpButton (pure $ pure "Contact last RBC") (pure True)
  , mkButton UpButton (pure $ pure "Use short number") (pure True)
  , mkButton UpButton (pure $ pure "Enter RBC data") (pure True) -- TODO: Image SE01
  , mkButton DelayButton (pure $ pure "Radio Network ID") (pure True)
  ]
