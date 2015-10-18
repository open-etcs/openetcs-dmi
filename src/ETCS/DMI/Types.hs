{-# LANGUAGE TemplateHaskell #-}

module ETCS.DMI.Types (
  ETCSMode (..), ETCSLevel (..), RadioSafeConnection(..),
  TrainBehavior(..), trainIsAtStandstill, trainMode, trainLevel, trainDriverIDIsValid,
  trainDataIsValid, trainLevelIsValid, trainRunningNumberIsValid,
  trainHasPendingEmergencyStop, trainHasCommunicationSession,
  trainIsNonLeading, trainIsPassiveShunting, trainModDriverIDAllowed,
  trainRadioSafeConnection, trainCommunicationSessionPending,
  ) where

import           Control.Lens
import           Reactive.Banana

data ETCSMode
  = FS -- ^ Full Supervision (FS)
  | LS -- ^ Limited Supervision (LS)
  | OS -- ^ On Sight (OS)
  | SR -- ^ Staff Responsible (SR)
  | SH -- ^ Shuting (SH)
  | UN -- ^ Unfitted (UN)
  | PS -- ^ Passive Shunting (PS)m
  | SL -- ^ Sleeping (SL)
  | SB -- ^ Stand By (SB)
  | TR -- ^ Trip (TR)
  | PT -- ^ Post Trip (PT)
  | SF -- ^ System Failure (SF)
  | IS -- ^ Isolation (IS)
  | NP -- ^ No Power (NP)
  | NL -- ^ Non Leading (NL)
  | SN -- ^ National System (SN)
  | RV -- ^ Reversing (RV)
  deriving (Eq, Show, Ord, Enum, Bounded)

makePrisms ''ETCSMode

data ETCSLevel = Level0 | NTC | Level1 | Level2 | Level3
               deriving (Eq, Show, Ord, Enum, Bounded)

makePrisms ''ETCSLevel

data RadioSafeConnection = ConnectionUp | NoConnection | ConnectionLost
               deriving (Eq, Show, Ord, Enum, Bounded)



data TrainBehavior =
  TrainBehavior {
    _trainIsAtStandstill              :: Behavior Bool,
    _trainMode                        :: Behavior ETCSMode,
    _trainLevel                       :: Behavior ETCSLevel,
    _trainDriverIDIsValid             :: Behavior Bool,
    _trainDataIsValid                 :: Behavior Bool,
    _trainLevelIsValid                :: Behavior Bool,
    _trainRunningNumberIsValid        :: Behavior Bool,
    _trainHasPendingEmergencyStop     :: Behavior Bool,
    _trainIsNonLeading                :: Behavior Bool,
    _trainModDriverIDAllowed          :: Behavior Bool,
    _trainRadioSafeConnection         :: Behavior RadioSafeConnection,
    _trainCommunicationSessionPending :: Behavior Bool
    }

makeLenses '' TrainBehavior


trainHasCommunicationSession :: Prism' (Behavior Bool) TrainBehavior
trainHasCommunicationSession = prism fromTB Left
  where fromTB tb = (== ConnectionUp ) <$> tb ^. trainRadioSafeConnection

trainIsPassiveShunting :: Prism' (Behavior Bool) TrainBehavior
trainIsPassiveShunting = prism fromTB Left
  where fromTB tb = (== PS) <$> tb ^. trainMode

