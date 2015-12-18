{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TemplateHaskell        #-}

module ETCS.DMI.Types where

import           Control.Lens                      hiding ((*~))
import           Data.Text                         (Text)
import           Numeric.Units.Dimensional.Prelude
import           Reflex


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

data ETCSLevel
  = Level0
  | NTC
  | Level1
  | Level2
  | Level3
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

makePrisms ''OnBoardData

instance Functor OnBoardData where
  fmap f (ValidData a) = _ValidData # f a
  fmap f (InvalidData a) = _InvalidData # f a
  fmap _ UnknownData = _UnknownData # ()

instance Applicative OnBoardData where
  pure = InvalidData
  (ValidData f)   <*> (ValidData a)   = _ValidData # f a
  (ValidData f)   <*> (InvalidData a) = _InvalidData # f a
  (InvalidData f) <*> (ValidData a)   = _InvalidData # f a
  (InvalidData f) <*> (InvalidData a) = _InvalidData # f a
  UnknownData <*> _ = _UnknownData # ()
  _ <*> UnknownData = _UnknownData # ()


-- | predicate if given 'OnBoardData' is '_ValidData'
onBoardIsValid :: OnBoardData a -> Bool
onBoardIsValid (ValidData _) = True
onBoardIsValid _ = False

-- | on '_ValidData' return value, else given default Value
onBoardDataWithDefault :: a -> OnBoardData a -> a
onBoardDataWithDefault _ (ValidData a)   = a
onBoardDataWithDefault _ (InvalidData a) = a
onBoardDataWithDefault a  UnknownData    = a

-- | The driver id
type DriverId = Text
type DriverIdData = OnBoardData DriverId

-- | The trains 'ETCSLevel'
type TrainLevelData = OnBoardData ETCSLevel

-- | A RBC identifier
type RBCId = Text

-- | The phone number of a RBC
type RBCPhoneNumber = Text

data RBCRecord =
  RBCRecord {
  _rbcId          :: RBCId,
  _rbcPhoneNumber :: RBCPhoneNumber
}
makePrisms ''RBCRecord
makeLenses ''RBCRecord

type RBCData = OnBoardData RBCRecord

-- | Train Category
data TrainCategory =
  PASS1 | PASS2 | PASS3 |
  TILT1 | TILT2 | TILT3 | TILT4 | TILT5 | TILT6 | TILT7 |
  FP1 | FP2 | FP3 | FP4 |
  FG1 | FG2 | FG3 | FG4
  deriving (Eq, Ord, Enum, Bounded, Show)

makePrisms ''TrainCategory

-- | the used UI colors of DMI
data UIColor = Grey | Yellow | Orange | Red | White | Black | DarkGrey | MediumGrey | DarkBlue
  deriving (Eq, Ord, Enum, Bounded, Show)


-- | ETCS defines 4 types of speed dials with different Vmax.
data SpeedDialType =
  SpeedDial140 | SpeedDial180 | SpeedDial250 | SpeedDial400
  deriving (Eq, Ord, Enum, Bounded, Show)


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

-- | a record storing train data
data TrainDataRecord =
  TrainData {
    _trainCategory         :: TrainCategory,
    _trainLength           :: Length Double,
    _trainBreakPercentage  :: Dimensionless Double,
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


data StatusInformation
  = NoS  -- | Normal Status information (NoS)
  | IndS -- | Indication Status infromation (IndS)
  | OvS  -- | Over-speed Status information (OvS)
  | WaS  -- | Warning Status information (WaS)
  | IntS -- | Intervention Status information (IntS)
  deriving (Eq, Ord, Enum, Bounded, Show)


data SuperVisionStatus
  = CSM -- | Ceiling Speed Monitoring (CSM)
  | PIM -- | Pre-Indication Monitoring (PIM)
  | TSM -- | Target Speed Monitoring (TSM)
  | RSM -- | Release Speed Monitoring (RSM)
  deriving (Eq, Ord, Enum, Bounded, Show)


data SdmData t =
  SdmData {
      _sdmVperm       :: Dynamic t (Velocity Double),
      _sdmVrelease    :: Dynamic t (Maybe (Velocity Double)),
      _sdmVtarget     :: Dynamic t (Velocity Double),
      _sdmVsbi        :: Dynamic t (Velocity Double),
      _sdmVwarn       :: Dynamic t (Velocity Double),
      _sdmVindication :: Dynamic t (Velocity Double),
      _sdmStatus      :: Dynamic t SuperVisionStatus
      }

makeClassy ''SdmData

data TrainBehavior t =
  TrainBehavior {
    _trainVelocity                    :: Dynamic t (Velocity Double),
    _trainNonLeadingInput             :: Dynamic t Bool,
    _trainEmergencyBreakActive        :: Dynamic t Bool,
    _trainServiceBreakActive          :: Dynamic t Bool,
    _trainIsNonLeading                :: Dynamic t Bool,
    _trainPassiveShuntingInput        :: Dynamic t Bool,
    _trainMode                        :: Dynamic t ETCSMode,
    _trainRadioSafeConnection         :: Dynamic t RadioSafeConnection,
    _trainCommunicationSessionPending :: Dynamic t Bool,
    _trainModDriverIDAllowed          :: Dynamic t Bool,
    _trainSpeedDial                   :: Dynamic t SpeedDialType,
    _trainLevel                       :: Dynamic t TrainLevelData,
    _trainDriverID                    :: Dynamic t DriverIdData,
    _trainData                        :: Dynamic t TrainData,
    _trainRunningNumber               :: Dynamic t RunningNumberData,
    _trainSdmData                     :: SdmData t
    }

makeClassy ''TrainBehavior

instance (Reflex t) => HasSdmData (TrainBehavior t) t where
  sdmData = trainSdmData

