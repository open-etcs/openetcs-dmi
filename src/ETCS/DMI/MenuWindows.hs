{-# LANGUAGE OverloadedStrings #-}

module ETCS.DMI.MenuWindows
       ( mkMainWindow, mkOverrideWindow, mkSpecialWindow, mkSettingsWindow
       , mkRBCContactWindow
       , module ETCS.DMI.MenuWindow
       ) where

import           Control.Lens
import           ETCS.DMI.Button
import           ETCS.DMI.Helpers
import           ETCS.DMI.MenuWindow
import           ETCS.DMI.Types
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.Frameworks

trainInMode :: ETCSMode -> TrainBehavior -> Behavior Bool
trainInMode m i = fmap (m ==) $ i ^. trainMode

trainInModes :: [ETCSMode] -> TrainBehavior -> Behavior Bool
trainInModes ms i = fmap (`elem` ms) $ i ^. trainMode



mkMainWindow :: (MonadIO m, IsNode p) =>
                TrainBehavior -> p -> Event Bool -> m (MomentIO MenuWindow)
mkMainWindow i parent visible = fmap fst $
  mkWidgetIO parent $ mkMenuWindow (pure "Main") visible
     [ mkButton UpButton (pure "Start") (bStartButtonEnabled i)
     , mkButton UpButton (pure "Driver ID") (bDriverIDButtonEnabled i)
     , mkButton UpButton (pure "Train Data") (bTrainDataButtonEnabled i)
     , mkButton UpButton (pure "") (pure False)
     , mkButton UpButton (pure "Level") (bLevelButtonEnabled i)
     , mkButton UpButton (pure "Train running Number") (pure True)
     , mkButton DelayButton (pure "Shunting") (pure True)
     , mkButton DelayButton (pure "Non-Leading") (pure True)
     , mkButton DelayButton (pure "Maintain Shunting") (pure False)
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

mkOverrideWindow :: (MonadIO m, IsNode p) => p -> Event Bool -> m (MomentIO (MenuWindow))
mkOverrideWindow parent visible = fmap fst $
  mkWidgetIO parent $ mkMenuWindow (pure "Override") visible
  [ mkButton UpButton (pure "EOA") (pure True)
  ]


mkSpecialWindow :: (MonadIO m, IsNode p) => p -> Event Bool -> m (MomentIO (MenuWindow))
mkSpecialWindow parent visible = fmap fst $
  mkWidgetIO parent $ mkMenuWindow (pure "Special") visible
  [ mkButton UpButton (pure "Ahension") (pure True)
  , mkButton UpButton (pure "SR speed / distance") (pure True)
  , mkButton DelayButton (pure "Train integrety") (pure True)
  ]

mkSettingsWindow :: (MonadIO m, IsNode p) => p -> Event Bool -> m (MomentIO (MenuWindow))
mkSettingsWindow parent visible = fmap fst $
  mkWidgetIO parent $ mkMenuWindow (pure "Settings") visible
  [ mkButton UpButton (pure "Language") (pure True) -- TODO: Image SE03
  , mkButton UpButton (pure "Volume") (pure True) -- TODO: Image SE02
  , mkButton UpButton (pure "Brightness") (pure True) -- TODO: Image SE01
  , mkButton UpButton (pure "System version") (pure True)
  , mkButton UpButton (pure "Set VBC") (pure True)
  , mkButton UpButton (pure "Remove VBC") (pure True)
  ]

mkRBCContactWindow :: (MonadIO m, IsNode p) => p -> Event Bool -> m (MomentIO (MenuWindow))
mkRBCContactWindow parent visible = fmap fst $
  mkWidgetIO parent $ mkMenuWindow (pure "RBC Contact") visible
  [ mkButton UpButton (pure "Contact last RBC") (pure True)
  , mkButton UpButton (pure "Use short number") (pure True)
  , mkButton UpButton (pure "Enter RBC data") (pure True) -- TODO: Image SE01
  , mkButton DelayButton (pure "Radio Network ID") (pure True)
  ]
