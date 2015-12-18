{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module ETCS.DMI.TrainBehavior  where


import           Control.Lens                      hiding ((*~))
import           ETCS.DMI.Types
import           Numeric.Units.Dimensional.Prelude
import qualified Prelude                           as Prelude
import           Reflex


trainBreaksActive :: (Reflex t, MonadHold t m) => TrainBehavior t -> m (Dynamic t Bool)
trainBreaksActive td =
  combineDyn (||) (td ^. trainServiceBreakActive) (td ^. trainEmergencyBreakActive)


{-

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

trainBreaksActive :: Getter TrainBehavior (Behavior Bool)
trainBreaksActive =
  to $ \td -> ((td ^. trainServiceBreakActive) `bOr` (td ^. trainEmergencyBreakActive))

-}
