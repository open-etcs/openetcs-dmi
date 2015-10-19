{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module ETCS.DMI.Types (
  ETCSMode (..), ETCSLevel (..), RadioSafeConnection(..),
  DriverId, TrainLevel, RBCId, RBCPhoneNumber, RBCData, TrainDataValue,
  RunningNumberValue, RunningNumber, TrainPositionValue, TrainPosition,
  OnBoardData, _ValidData, _InvalidData, _UnknownData,
  TrainBehavior(..), TrainDataValue(..), trainPassiveShuntingInput,
  trainIsAtStandstill, trainMode, trainLevel, trainDriverIDIsValid,
  trainDataIsValid, trainLevelIsValid, trainRunningNumberIsValid,
  trainEmergencyStop, trainHasCommunicationSession, trainNonLeadingInput,
  trainIsNonLeading, trainIsPassiveShunting, trainModDriverIDAllowed,
  trainRadioSafeConnection, trainCommunicationSessionPending, trainVelocity,
  trainInLevel, trainInLevels, trainInMode, trainInModes
  ) where

import           Control.Lens                         hiding ((*~))
import           Data.Text                            (Text)
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

makePrisms ''RadioSafeConnection


-- | Describes onboard data validity. Data is either '_UnknownData',
--   '_InvalidData' or '_ValidData'. It is a 'Functor' as well as an
--   'Applicative' where 'pure' returns a '_InvalidData' of given 'a'.
data OnBoardData a
  = ValidData a   -- ^ The stored value is known to be correct.
  | InvalidData a -- ^ The stored value may be wrong.
  | UnknownData   -- ^ No stored value stored.

instance Functor OnBoardData where
  fmap f (ValidData a) = ValidData (f a)
  fmap f (InvalidData a) = InvalidData (f a)
  fmap _ UnknownData = UnknownData

instance Applicative OnBoardData where
  pure = InvalidData
  (ValidData f)   <*> (ValidData a)   = ValidData $ f a
  (ValidData f)   <*> (InvalidData a) = InvalidData $ f a
  (InvalidData f) <*> (ValidData a)   = InvalidData $ f a
  (InvalidData f) <*> (InvalidData a) = InvalidData $ f a
  UnknownData <*> _ = UnknownData
  _ <*> UnknownData = UnknownData

makePrisms ''OnBoardData

-- The driver id
type DriverId = OnBoardData Text

-- The trains 'ETCSLevel'
type TrainLevel = OnBoardData ETCSLevel

-- A RBC identifier
type RBCId = Text

-- The phone number of a RBC
type RBCPhoneNumber = Text

-- | 'Either' a 'RBCId', or a 'RBCPhoneNumber'
type RBCData = OnBoardData (Either RBCId RBCPhoneNumber)



data TrainDataValue = TrainData

makeLenses ''TrainDataValue

type TrainData = OnBoardData TrainDataValue


type RunningNumberValue = Int
type RunningNumber = OnBoardData RunningNumberValue

type TrainPositionValue = Length Double
data TrainPosition = OnBoardData TrainPositionValue

data TrainBehavior =
  TrainBehavior {
    _trainVelocity                    :: Behavior (Velocity Double),
    _trainNonLeadingInput             :: Behavior Bool,
    _trainPassiveShuntingInput        :: Behavior Bool,
    _trainMode                        :: Behavior ETCSMode,
    _trainLevel                       :: Behavior TrainLevel,
    _trainEmergencyStop               :: Behavior Bool,
    _trainIsNonLeading                :: Behavior Bool,
    _trainRadioSafeConnection         :: Behavior RadioSafeConnection,
    _trainCommunicationSessionPending :: Behavior Bool,
    _trainModDriverIDAllowed          :: Behavior Bool,
    _trainDriverID                    :: Behavior DriverId,
    _trainData                        :: Behavior TrainData,
    _trainRunningNumber               :: Behavior RunningNumber
    }

makeLenses ''TrainBehavior



behaviorTrainDataIsValid ::
  Getter TrainBehavior (Behavior (OnBoardData a)) ->
  Getter TrainBehavior (Behavior Bool)
behaviorTrainDataIsValid g = to fromTB
  where fromTB tb = isValidData <$> tb ^. g
        isValidData (ValidData _) = True
        isValidData _ = False


behaviorTrainDataValue ::
  Getter TrainBehavior (Behavior (OnBoardData a)) ->
  Getter TrainBehavior (Behavior (Maybe a))
behaviorTrainDataValue g = to fromTB
  where fromTB tb = isValidData <$> tb ^. g
        isValidData (ValidData a) = pure a
        isValidData (InvalidData a) = pure a
        isValidData _ = Nothing

trainDriverIDIsValid :: Getter TrainBehavior (Behavior Bool)
trainDriverIDIsValid = behaviorTrainDataIsValid trainDriverID

trainLevelIsValid :: Getter TrainBehavior (Behavior Bool)
trainLevelIsValid = behaviorTrainDataIsValid trainLevel

trainRunningNumberIsValid :: Getter TrainBehavior (Behavior Bool)
trainRunningNumberIsValid = behaviorTrainDataIsValid trainRunningNumber

trainDataIsValid :: Getter TrainBehavior (Behavior Bool)
trainDataIsValid = behaviorTrainDataIsValid trainData

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
trainInLevel m = to $ fmap _isLevel . view (behaviorTrainDataValue trainLevel)
  where _isLevel Nothing  = False
        _isLevel (Just l) = m == l

trainInLevels :: [ETCSLevel] -> Getter TrainBehavior (Behavior Bool)
trainInLevels ms = to $ fmap _inLevels . view (behaviorTrainDataValue trainLevel)
  where _inLevels Nothing  = False
        _inLevels (Just l) = l `elem` ms
