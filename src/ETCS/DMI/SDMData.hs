{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module ETCS.DMI.SDMData where


import           Control.Lens                         hiding ((*~))
import           ETCS.DMI.Helpers
import           ETCS.DMI.TrainBehavior
import           ETCS.DMI.Types
import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              ()
import           Reactive.Banana

makeLenses ''SDMData


trainSDMVrelease :: Getter TrainBehavior (Behavior (Maybe (Velocity Double)))
trainSDMVrelease = to $ view sdmVrelease . view trainSDMData

trainSDMVperm :: Getter TrainBehavior (Behavior (Velocity Double))
trainSDMVperm = to $ view sdmVperm . view trainSDMData

trainSDMVtarget :: Getter TrainBehavior (Behavior (Velocity Double))
trainSDMVtarget = to $ view sdmVtarget . view trainSDMData

trainSDMVsbi :: Getter TrainBehavior (Behavior (Velocity Double))
trainSDMVsbi = to $ view sdmVsbi . view trainSDMData
