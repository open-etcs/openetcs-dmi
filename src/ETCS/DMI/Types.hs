{-# LANGUAGE TemplateHaskell #-}

module ETCS.DMI.Types (
  ETCSMode (..), ETCSLevel (..), TrainData,
  NationalValues,
  TrainBehavior, trainIsAtStandstill, trainMode, trainLevel, trainDriverIDIsValid,
  trainDataIsValid, trainLevelIsValid, trainRunningNumberIsValid,
  trainHasPendingEmergencyStop, trainNationalValues, trainHasCommunicationSession,
  trainIsNonLeading, trainIsPassiveShunting, trainModDriverIDAllowed,
  Button, _Button, buttonE, buttonCleanup, ButtonType(..),
  WindowMenuButtonId (..),
  MenuWindow, _MenuWindow, menuWinE, menuWinCleanup
  ) where

import           Control.Lens
import           FRP.Sodium

data ETCSMode = SB | PT | SR | SH | FS | LS | OS | NL| UN | SN
              deriving (Eq, Show, Ord, Enum, Bounded)

makePrisms ''ETCSMode

data ETCSLevel = Level0 | NTC | Level1 | Level2 | Level3
               deriving (Eq, Show, Ord, Enum, Bounded)

makePrisms ''ETCSLevel

data TrainData = TrainData

makeLenses ''TrainData

data NationalValues = NationalValues {

}
makeLenses ''NationalValues

data TrainBehavior =
  TrainBehavior {
    _trainIsAtStandstill          :: Behavior Bool,
    _trainMode                    :: Behavior ETCSMode,
    _trainLevel                   :: Behavior ETCSLevel,
    _trainDriverIDIsValid         :: Behavior Bool,
    _trainDataIsValid             :: Behavior Bool,
    _trainLevelIsValid            :: Behavior Bool,
    _trainRunningNumberIsValid    :: Behavior Bool,
    _trainHasPendingEmergencyStop :: Behavior Bool,
    _trainNationalValues          :: Behavior NationalValues,
    _trainHasCommunicationSession :: Behavior Bool,
    _trainIsNonLeading            :: Behavior Bool,
    _trainIsPassiveShunting       :: Behavior Bool,
    _trainModDriverIDAllowed      :: Behavior Bool
    }

makeLenses '' TrainBehavior



data Button t =
  Button {
    _buttonE       :: Event t,
    _buttonCleanup :: IO ()
    }

makeLenses ''Button
makePrisms ''Button


data ButtonType = UpButton | DownButton | DelayButton
  deriving (Eq, Ord, Show, Enum, Bounded)



data WindowMenuButtonId =
  B1 | B2 | B3 | B4 | B5 | B6 | B7 | B8 | B9 | B10
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data MenuWindow =
  MenuWindow {
    _menuWinE       :: Event WindowMenuButtonId,
    _menuWinCleanup :: IO ()
    }

makeLenses ''MenuWindow
makePrisms ''MenuWindow
