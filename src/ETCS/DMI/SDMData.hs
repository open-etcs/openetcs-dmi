{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module ETCS.DMI.SDMData where


import           Control.Lens                         hiding ((*~))
import           ETCS.DMI.TrainBehavior
import           ETCS.DMI.Types
import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              ()
import           Reactive.Banana
import           Reactive.Banana.DOM.Widget
import           Reactive.Banana.Frameworks


makeLenses ''SDMData


trainSDMVrelease :: Getter TrainBehavior (Behavior (Maybe (Velocity Double)))
trainSDMVrelease = trainSDMData . sdmVrelease

trainSDMVperm :: Getter TrainBehavior (Behavior (Velocity Double))
trainSDMVperm = trainSDMData . sdmVperm

trainSDMVtarget :: Getter TrainBehavior (Behavior (Velocity Double))
trainSDMVtarget = trainSDMData . sdmVtarget

trainSDMVsbi :: Getter TrainBehavior (Behavior (Velocity Double))
trainSDMVsbi = trainSDMData .  sdmVsbi

trainSDMVwarn :: Getter TrainBehavior (Behavior (Velocity Double))
trainSDMVwarn = trainSDMData . sdmVwarn

trainSDMVindication :: Getter TrainBehavior (Behavior (Velocity Double))
trainSDMVindication =  trainSDMData .sdmVindication

trainSDMstatus :: Getter TrainBehavior (Behavior SuperVisionStatus)
trainSDMstatus = trainSDMData . sdmStatus


informationStatus :: TrainBehavior -> MomentIO (Behavior StatusInformation)
informationStatus td = do
  csm <- csmInformationStatus td
  pim <- pimInformationStatus td
  tsm <- tsmInformationStatus td
  rsm <- rsmInformationStatus td
  return $ switchMode <$> td ^. trainSDMstatus <*> csm <*> pim <*> tsm <*> rsm
  where switchMode CSM csm _ _ _ = csm
        switchMode PIM _ pim _ _ = pim
        switchMode TSM _ _ tsm _ = tsm
        switchMode RSM _ _ _ rsm = rsm


csmInformationStatus :: TrainBehavior -> MomentIO (Behavior StatusInformation)
csmInformationStatus td =
  let p = td ^. trainSDMVperm
      v = td ^. trainVelocity
      w = td ^. trainSDMVwarn
      sbi = td ^. trainSDMVsbi
      nos = (<=) <$> v <*> p
      ovs = not <$> nos
      breaks = td ^. trainBreaksActive
  in do
    was <-  rsFlipFlop ((>) <$> v <*> w) ((<=) <$> v <*> p)
    ints <- rsFlipFlop ((>) <$> v <*> sbi) (not <$> breaks)
    return $ status <$> nos <*> ovs <*> was <*> ints
      where status _ _ _ True = IntS
            status _ _ True _ = WaS
            status _ True _ _ = OvS
            status _ _ _ _    = NoS

pimInformationStatus :: TrainBehavior -> MomentIO (Behavior StatusInformation)
pimInformationStatus = csmInformationStatus

tsmInformationStatus :: TrainBehavior -> MomentIO (Behavior StatusInformation)
tsmInformationStatus td =
  let p = td ^. trainSDMVperm
      v = td ^. trainVelocity
      t = td ^. trainSDMVtarget
      i = td ^. trainSDMVindication
      w = td ^. trainSDMVwarn
      breaks = td ^. trainBreaksActive
      sbi = td ^. trainSDMVsbi
      nos = (<=) <$> v <*> i
      ovs = (>) <$> v <*> p
  in do
    inds <- rsFlipFlop ((>) <$> v <*> i) ((<) <$> v <*> t)
    was <-  rsFlipFlop ((>) <$> v <*> w) ((<=) <$> v <*> p)
    ints <- rsFlipFlop ((>) <$> v <*> sbi) (not <$> breaks)
    return $ status <$> nos <*> inds <*> ovs <*> was <*> ints
      where status _ _ _ _ True = IntS
            status _ _ _ True _ = WaS
            status _ _ True _ _ = OvS
            status _ True _ _ _ = IndS
            status _ _ _ _ _    = NoS

rsmInformationStatus :: TrainBehavior -> MomentIO (Behavior StatusInformation)
rsmInformationStatus td =
  let r = td ^. trainSDMVrelease
      v = td ^. trainVelocity
      breaks = td ^. trainBreaksActive
      test p a (Just b) = a `p` b
      test _ _ Nothing  = False
      inds = test (<=) <$> v <*> r
  in do
    ints <- rsFlipFlop (not <$> inds) (not <$> breaks)
    return $ status <$> inds <*> ints
      where status _ True = IntS
            status _ _    = IndS

