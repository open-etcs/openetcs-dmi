{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module ETCS.DMI.Types where

import           Control.Lens                         hiding ((*~))
import           Data.Text                            (Text)
import           Numeric.Units.Dimensional.TF.Prelude
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


onBoardIsValid :: OnBoardData a -> Bool
onBoardIsValid (ValidData _) = True
onBoardIsValid _ = False

onBoardDataWithDefault :: a -> OnBoardData a -> a
onBoardDataWithDefault _ (ValidData a)   = a
onBoardDataWithDefault _ (InvalidData a) = a
onBoardDataWithDefault a  UnknownData    = a



makePrisms ''OnBoardData

-- The driver id
type DriverId = Text
type DriverIdData = OnBoardData DriverId

-- The trains 'ETCSLevel'
type TrainLevelData = OnBoardData ETCSLevel

-- A RBC identifier
type RBCId = Text

-- The phone number of a RBC
type RBCPhoneNumber = Text

data RBCRecord =
  RBCRecord {
  _rbcId          :: RBCId,
  _rbcPhoneNumber :: RBCPhoneNumber
}

makeLenses ''RBCRecord

type RBCData = OnBoardData RBCRecord

data TrainCategory =
  PASS1 | PASS2 | PASS3 |
  TILT1 | TILT2 | TILT3 | TILT4 | TILT5 | TILT6 | TILT7 |
  FP1 | FP2 | FP3 | FP4 |
  FG1 | FG2 | FG3 | FG4
  deriving (Eq, Ord, Enum, Bounded, Show)

makePrisms ''TrainCategory




data LoadingGauge
  = LoadingGauge1 | LoadingGaugeA | LoadingGaugeB | LoadingGaugeC
  | LoadingGaugeOutOfGC
  deriving (Eq, Ord, Enum, Bounded, Show)

data AxleLoadCategory
  = AxleLoadA
  | AxleLoadB1 | AxleLoadB2
  | AxleLoadC2 | AxleLoadC3 | AxleLoadC4
  | AxleLoadD2 | AxleLoadD3 | AxleLoadD4 | AxleLoadD4XL
  | AxleLoadE4 | AxleLoadE5
  deriving (Eq, Ord, Enum, Bounded, Show)

data TrainDataRecord =
  TrainData {
    _trainCategory         :: TrainCategory,
    _trainLength           :: Length Int,
    _trainBreakPercentage  :: Dimensionless Int,
    _trainMaxSpeed         :: Velocity Double,
    _trainAxleLoadCategory :: AxleLoadCategory,
    _trainAirTight         :: Bool,
    _trainLoadingGauge     :: LoadingGauge
    } deriving (Eq, Show)

makeLenses ''TrainDataRecord

type TrainData = OnBoardData TrainDataRecord

type RunningNumber = Int
type RunningNumberData = OnBoardData RunningNumber

type TrainPosition = Length Double
data TrainPositionData = OnBoardData TrainPosition

data TrainBehavior =
  TrainBehavior {
    _trainVelocity                    :: Behavior (Velocity Double),
    _trainNonLeadingInput             :: Behavior Bool,
    _trainEmergencyStop               :: Behavior Bool,
    _trainIsNonLeading                :: Behavior Bool,
    _trainPassiveShuntingInput        :: Behavior Bool,
    _trainMode                        :: Behavior ETCSMode,
    _trainRadioSafeConnection         :: Behavior RadioSafeConnection,
    _trainCommunicationSessionPending :: Behavior Bool,
    _trainModDriverIDAllowed          :: Behavior Bool,
    _trainLevel                       :: Behavior TrainLevelData,
    _trainDriverID                    :: Behavior DriverIdData,
    _trainData                        :: Behavior TrainData,
    _trainRunningNumber               :: Behavior RunningNumberData
    }

