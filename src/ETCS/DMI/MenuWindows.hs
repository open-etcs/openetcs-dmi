{-# LANGUAGE OverloadedStrings #-}

module ETCS.DMI.MenuWindows
       ( mkMainWindow, mkOverrideWindow, mkSpecialWindow, mkSettingsWindow
       , mkRBCContactWindow
       , menuWinE, menuWinCleanup
       ) where

import           Control.Lens
import           ETCS.DMI.Button
import           ETCS.DMI.Helpers
import           ETCS.DMI.MenuWindow
import           ETCS.DMI.Types
import           FRP.Sodium
import           GHCJS.DOM.Types     (IsDocument, IsNode)

trainInMode :: ETCSMode -> TrainBehavior -> Behavior Bool
trainInMode m i = fmap (m ==) $ i ^. trainMode

trainInModes :: [ETCSMode] -> TrainBehavior -> Behavior Bool
trainInModes ms i = fmap (`elem` ms) $ i ^. trainMode

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


mkMainWindow :: (IsDocument d, IsNode p) =>
                TrainBehavior -> d -> p -> Event Bool -> IO MenuWindow
mkMainWindow i doc parent visible =
  mkMenuWindow doc parent (pure "Main") visible
     [ (UpButton, pure "Start", bStartButtonEnabled i)
     , (UpButton, pure "Driver ID", bDriverIDButtonEnabled i)
     , (UpButton, pure "Train Data", bTrainDataButtonEnabled i)
     , (UpButton, pure "", pure False)
     , (UpButton, pure "Level", bLevelButtonEnabled i)
     , (UpButton, pure "Train running Number", pure True)
     , (DelayButton, pure "Shunting", pure True)
     , (DelayButton, pure "Non-Leading", pure True)
     , (DelayButton, pure "Maintain Shunting", pure False)
     ]


mkOverrideWindow :: (IsDocument d, IsNode p) => d -> p -> Event Bool -> IO MenuWindow
mkOverrideWindow doc parent visible =
  mkMenuWindow doc parent (pure "Override") visible
  [ (UpButton, pure "EOA", pure True)
  ]


mkSpecialWindow :: (IsDocument d, IsNode p) => d -> p -> Event Bool -> IO MenuWindow
mkSpecialWindow doc parent visible =
  mkMenuWindow doc parent (pure "Special") visible
  [ (UpButton, pure "Ahension", pure True)
  , (UpButton, pure "SR speed / distance", pure True)
  , (DelayButton, pure "Train integrety", pure True)
  ]

mkSettingsWindow :: (IsDocument d, IsNode p) => d -> p -> Event Bool -> IO MenuWindow
mkSettingsWindow doc parent visible =
  mkMenuWindow doc parent (pure "Settings") visible
  [ (UpButton, pure "Language", pure True) -- TODO: Image SE03
  , (UpButton, pure "Volume", pure True) -- TODO: Image SE02
  , (UpButton, pure "Brightness", pure True) -- TODO: Image SE01
  , (UpButton, pure "System version", pure True)
  , (UpButton, pure "Set VBC", pure True)
  , (UpButton, pure "Remove VBC", pure True)
  ]

mkRBCContactWindow :: (IsDocument d, IsNode p) => d -> p -> Event Bool -> IO MenuWindow
mkRBCContactWindow doc parent visible =
  mkMenuWindow doc parent (pure "RBC Contact") visible
  [ (UpButton, pure "Contact last RBC", pure True)
  , (UpButton, pure "Use short number", pure True)
  , (UpButton, pure "Enter RBC data", pure True) -- TODO: Image SE01
  , (DelayButton, pure "Radio Network ID", pure True)
  ]
