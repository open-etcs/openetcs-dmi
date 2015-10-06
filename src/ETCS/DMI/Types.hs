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


data TrainBehavior t =
  TrainBehavior {
    _trainIsAtStandstill          :: Behavior t Bool,
    _trainMode                    :: Behavior t ETCSMode,
    _trainLevel                   :: Behavior t ETCSLevel,
    _trainDriverIDIsValid         :: Behavior t Bool,
    _trainDataIsValid             :: Behavior t Bool,
    _trainLevelIsValid            :: Behavior t Bool,
    _trainRunningNumberIsValid    :: Behavior t Bool,
    _trainHasPendingEmergencyStop :: Behavior t Bool,
    _trainHasCommunicationSession :: Behavior t Bool,
    _trainIsNonLeading            :: Behavior t Bool,
    _trainIsPassiveShunting       :: Behavior t Bool,
    _trainModDriverIDAllowed      :: Behavior t Bool
    }

makeLenses '' TrainBehavior



data ButtonType = UpButton | DownButton | DelayButton
  deriving (Eq, Ord, Show, Enum, Bounded)


