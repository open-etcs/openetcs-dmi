{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module ETCS.DMI.Types (
  ETCSMode (..), ETCSLevel (..), RadioSafeConnection(..),
  TrainBehavior(..), trainIsAtStandstill, trainMode, trainLevel, trainDriverIDIsValid,
  trainDataIsValid, trainLevelIsValid, trainRunningNumberIsValid,
  trainHasPendingEmergencyStop, trainHasCommunicationSession,
  trainIsNonLeading, trainIsPassiveShunting, trainModDriverIDAllowed,
  trainRadioSafeConnection, trainCommunicationSessionPending, trainVelocity,
  trainInLevel, trainInLevels, trainInMode, trainInModes
  ) where

import           Control.Lens                         hiding ((*~))
import           ETCS.DMI.Helpers
import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              ()
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
    _trainVelocity                    :: Behavior (Velocity Double),
    _trainMode                        :: Behavior ETCSMode,
    _trainLevel                       :: Behavior ETCSLevel,
    _trainHasPendingEmergencyStop     :: Behavior Bool,
    _trainIsNonLeading                :: Behavior Bool,
    _trainRadioSafeConnection         :: Behavior RadioSafeConnection,
    _trainCommunicationSessionPending :: Behavior Bool,
    _trainModDriverIDAllowed          :: Behavior Bool,
    _trainDriverIDIsValid             :: Behavior Bool,
    _trainDataIsValid                 :: Behavior Bool,
    _trainLevelIsValid                :: Behavior Bool,
    _trainRunningNumberIsValid        :: Behavior Bool
    }

makeLenses ''TrainBehavior


trainHasCommunicationSession :: Getter TrainBehavior (Behavior Bool)
trainHasCommunicationSession = to fromTB
  where fromTB tb = (== ConnectionUp ) <$> tb ^. trainRadioSafeConnection

trainIsPassiveShunting :: Getter TrainBehavior (Behavior Bool)
trainIsPassiveShunting = to fromTB
  where fromTB tb = (== PS) <$> tb ^. trainMode

trainIsAtStandstill :: Getter TrainBehavior (Behavior Bool)
trainIsAtStandstill = to fromTB
  where fromTB tb = (== (0 *~ kmh)) <$> tb ^. trainVelocity



trainInMode :: ETCSMode -> Getter TrainBehavior (Behavior Bool)
trainInMode m = to $ fmap (m ==) . view trainMode

trainInModes :: [ETCSMode] -> Getter TrainBehavior (Behavior Bool)
trainInModes ms = to $ fmap (`elem` ms) . view trainMode

trainInLevel :: ETCSLevel -> Getter TrainBehavior (Behavior Bool)
trainInLevel m = to $ fmap (m ==) . view trainLevel

trainInLevels :: [ETCSLevel] -> Getter TrainBehavior (Behavior Bool)
trainInLevels ms = to $ fmap (`elem` ms) . view trainLevel



