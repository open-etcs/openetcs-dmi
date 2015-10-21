{-# LANGUAGE TypeFamilies #-}


module ETCS.DMI.StartupSequence where

import           Control.Lens
import           ETCS.DMI.Helpers
import           ETCS.DMI.TrainBehavior
import           ETCS.DMI.Types
import           ETCS.DMI.Windows.MainWindow
import           GHCJS.DOM.Node              (appendChild)
import           GHCJS.DOM.Types             (castToElement)
import           Reactive.Banana.DOM
import           Reactive.Banana.DOM.Widget


data StartupSequence = StartupSequence

mkStartupSequence :: TrainBehavior -> WidgetInput StartupSequence
mkStartupSequence = MkStartupSequence

instance IsWidget StartupSequence where
  data WidgetInput StartupSequence = MkStartupSequence {
    _startupSeqTrainBehavior :: TrainBehavior
    }

  mkWidgetInstance parent wi =
    let i = _startupSeqTrainBehavior wi
        isConnectedOrPendingInSB =
          bAnd (i ^. trainInMode SB) $
          (i ^. trainHasCommunicationSession) `bOr`
          (i ^. trainCommunicationSessionPending)
        mkMainWin p = mkSubWidget p . mkMainWindow i
                    isConnectedOrPendingInSB
                    isConnectedOrPendingInSB
                    (pure True)
    in do
      doc <- _getOwnerDocument parent
      container <- _createDivElement doc
      mainWinW <- mkMainWin container (pure True)

      _ <- appendChild parent $ pure container

      return (StartupSequence, castToElement container)





