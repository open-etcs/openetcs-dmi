{-# LANGUAGE Trustworthy #-}

module ETCS.DMI.SDMData (informationStatus) where


import           Control.Lens                      hiding ((*~))
import           ETCS.DMI.TrainBehavior
import           ETCS.DMI.Types
import           ETCS.DMI.Helpers
import           Numeric.Units.Dimensional.Prelude
import           Prelude                           ()
import           Reflex
import           Control.Monad.Fix



informationStatus :: (Reflex t, MonadSample t m, MonadHold t m, MonadFix m) =>
                     TrainBehavior t -> m (Dynamic t StatusInformation)
informationStatus td = do
  csm <- csmInformationStatus td
  pim <- pimInformationStatus td
  tsm <- tsmInformationStatus td
  rsm <- rsmInformationStatus td
  let status = td ^. sdmStatus
      statusInfB = switchMode <$> current status <*> current csm <*> current pim <*> current tsm <*> current rsm
      statusInfE = attach statusInfB $
                   leftmost [ const () <$> updated status
                            , const () <$> updated csm
                            , const () <$> updated pim
                            , const () <$> updated tsm
                            , const () <$> updated rsm
                            ]
  statusInf <- sample statusInfB
  nubDyn <$> holdDyn statusInf (fst <$> statusInfE)
  where switchMode CSM csm _ _ _ = csm
        switchMode PIM _ pim _ _ = pim
        switchMode TSM _ _ tsm _ = tsm
        switchMode RSM _ _ _ rsm = rsm




csmInformationStatus :: (Reflex t, MonadSample t m, MonadHold t m, MonadFix m) =>
                        TrainBehavior t -> m (Dynamic t StatusInformation)
csmInformationStatus td =
  let p = td ^. sdmVperm
      v = td ^. trainVelocity
      w = td ^. sdmVwarn
      sbi = td ^. sdmVsbi
  in do
    nos <- combineDyn (<=) v p
    ovs <- mapDyn not nos
    setWas <- combineDyn (>) v w
    resetWas <- combineDyn (<=) v p

    was <-  srFlipFlop setWas resetWas
    
    resetInts <- trainBreaksActive td >>= mapDyn not
    setInts <- combineDyn (>) v sbi
    ints <- srFlipFlop setInts resetInts

    let csmB = status <$> current nos <*> current ovs <*> current was <*> current ints
        csmE = attach csmB $
               leftmost [ updated nos, updated ovs, updated was, updated ints]
    csm <- sample csmB
    nubDyn <$> holdDyn csm (fst <$> csmE)
      where status _ _ _ True = IntS
            status _ _ True _ = WaS
            status _ True _ _ = OvS
            status _ _ _ _    = NoS


pimInformationStatus :: (Reflex t, MonadSample t m, MonadHold t m, MonadFix m) =>
                        TrainBehavior t -> m (Dynamic t StatusInformation)
pimInformationStatus = csmInformationStatus


tsmInformationStatus :: (Reflex t, MonadSample t m, MonadHold t m, MonadFix m) =>
                        TrainBehavior t -> m (Dynamic t StatusInformation)
tsmInformationStatus td =
  let p = td ^. sdmVperm
      v = td ^. trainVelocity
      t = td ^. sdmVtarget
      i = td ^. sdmVindication
      w = td ^. sdmVwarn
      sbi = td ^. sdmVsbi
  in do
    breaks <- trainBreaksActive td
    notBreaks <- mapDyn not breaks
    nos <- combineDyn (<=) v i
    ovs <- combineDyn (>) v p
    
    setInds <- combineDyn (>) v i
    resetInds <- combineDyn (<) v t
    inds <- srFlipFlop setInds resetInds

    setWas <- combineDyn (>) v w
    resetWas <- combineDyn (<=) v p
    was <-  srFlipFlop setWas resetWas

    setInts <- combineDyn (>) v sbi
    ints <- srFlipFlop setInts notBreaks


    let tsmB = status <$> current nos <*>
               current inds <*> current ovs <*> current was <*> current ints
        tsmE = attach tsmB $
               leftmost [ updated nos, updated inds, updated ovs
                        , updated was, updated ints]
    tsm <- sample tsmB
    nubDyn <$> holdDyn tsm (fst <$> tsmE)    
      where status _ _ _ _ True = IntS
            status _ _ _ True _ = WaS
            status _ _ True _ _ = OvS
            status _ True _ _ _ = IndS
            status _ _ _ _ _    = NoS




rsmInformationStatus :: (Reflex t, MonadSample t m, MonadHold t m, MonadFix m) =>
                        TrainBehavior t -> m (Dynamic t StatusInformation)
rsmInformationStatus td =
  let rM = td ^. sdmVrelease
      v = td ^. trainVelocity
  in do
    r <- mapDyn (maybe ((-1) *~ kmh) id) rM
    breaks <- trainBreaksActive td
    inds <- combineDyn (<=) v r

    setInts <- mapDyn not inds
    resetInts <- mapDyn not breaks    
    ints <- srFlipFlop setInts resetInts

    nubDyn <$> combineDyn status inds ints
      where status _ True = IntS
            status _ _    = IndS
