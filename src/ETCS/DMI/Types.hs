{-# LANGUAGE TemplateHaskell #-}

module ETCS.DMI.Types (
  ETCSMode (..), ETCSLevel (..),
  TrainBehavior(..), trainIsAtStandstill, trainMode, trainLevel, trainDriverIDIsValid,
  trainDataIsValid, trainLevelIsValid, trainRunningNumberIsValid,
  trainHasPendingEmergencyStop, trainHasCommunicationSession,
  trainIsNonLeading, trainIsPassiveShunting, trainModDriverIDAllowed,
  ButtonType(..)
  ) where

import           Control.Lens
import           Reactive.Banana

data ETCSMode = SB | PT | SR | SH | FS | LS | OS | NL| UN | SN
              deriving (Eq, Show, Ord, Enum, Bounded)

makePrisms ''ETCSMode

data ETCSLevel = Level0 | NTC | Level1 | Level2 | Level3
               deriving (Eq, Show, Ord, Enum, Bounded)

makePrisms ''ETCSLevel


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
    _trainHasCommunicationSession :: Behavior Bool,
    _trainIsNonLeading            :: Behavior Bool,
    _trainIsPassiveShunting       :: Behavior Bool,
    _trainModDriverIDAllowed      :: Behavior Bool
    }

makeLenses '' TrainBehavior



data ButtonType = UpButton | DownButton | DelayButton
  deriving (Eq, Ord, Show, Enum, Bounded)


