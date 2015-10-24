{-# LANGUAGE TypeFamilies #-}


module ETCS.DMI.StartupSequence where

import           Control.Lens
import           Control.Monad.Writer
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           ETCS.DMI.Helpers
import           ETCS.DMI.TrainBehavior
import           ETCS.DMI.Types
import           ETCS.DMI.Windows.DriverIdWindow
import           ETCS.DMI.Windows.MainWindow
import           GHCJS.DOM.Node                  (IsNode, appendChild)
import           GHCJS.DOM.Types                 (castToElement)
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.DOM.Widget
import           Reactive.Banana.Frameworks


data StartupSequence = StartupSequence

mkStartupSequence :: TrainBehavior -> WidgetInput StartupSequence
mkStartupSequence = MkStartupSequence


data WindowId =
  MainWindowId | DriverIdWindowId
  deriving (Eq, Ord, Enum, Bounded)

type HideWindowMap = Map WindowId (Bool -> IO ())

hideAllWindowsBut :: HideWindowMap -> WindowId -> IO ()
hideAllWindowsBut m b =
  let (as, bs) = Map.mapEitherWithKey (\k a -> if k == b then Left a else Right a) m
  in sequence_ $
     ((($ True) . snd) <$> Map.toList bs) ++ ((($ False)  . snd) <$> Map.toList as)


data StartupSequenceState =
  S0 | S1 | S1_1 | S1_2 | S2 | S3_1 | S3_2 | S3_3 | S10
  deriving (Eq, Ord, Enum, Bounded)

instance IsWidget StartupSequence where
  data WidgetInput StartupSequence = MkStartupSequence {
    _startupSeqTrainBehavior :: TrainBehavior
    }

  mkWidgetInstance parent wi =
    let i = _startupSeqTrainBehavior wi
    in do
      container <- _getOwnerDocument parent >>= _createDivElement

      (bState, fireState) <- lift $ newBehavior S0

      (mainWin, hideMainWin) <- mkMainWin i container
      (driverIdWin, hideDriverIdWin) <- mkDriverIdWin i container

      let hideAllWindowsBut' =
            hideAllWindowsBut $ Map.fromList
            [ (MainWindowId, hideMainWin)
            , (DriverIdWindowId, hideDriverIdWin)
            ]

      -- handle main window events
      let onMainWinE (Left ()) = return ()
          onMainWinE (Right ButtonDriverId) = hideAllWindowsBut' DriverIdWindowId
      lift . reactimate $
        onMainWinE <$> (widgetEvent $ fromWidgetInstance mainWin)


      _ <- appendChild parent $ pure container
      return (StartupSequence, castToElement container)



mkMainWin :: (IsNode n) => TrainBehavior -> n ->
             ReactiveDom (WidgetInstance MainWindow, Handler Bool)
mkMainWin i container =
  let isConnectedOrPendingInSB =
        bAnd (i ^. trainInMode SB) $
        (i ^. trainHasCommunicationSession) `bOr`
        (i ^. trainCommunicationSessionPending)
  in do
    (bMainWinHidden, hideMainWin) <- lift $ newBehavior False
    mainWinW <- mkSubWidget container $
                mkMainWindow i isConnectedOrPendingInSB isConnectedOrPendingInSB
                (pure True) (not <$> bMainWinHidden)
    return (mainWinW, hideMainWin)


mkDriverIdWin :: (IsNode n) => TrainBehavior -> n ->
             ReactiveDom (WidgetInstance DriverIdWindow, Handler Bool)
mkDriverIdWin i container =
  let isConnectedOrPendingInSB =
        bAnd (i ^. trainInMode SB) $
        (i ^. trainHasCommunicationSession) `bOr`
        (i ^. trainCommunicationSessionPending)
  in do
    (bDriverIdWinHidden, hideDriverIdWin) <- lift $ newBehavior True
    mainWinW <- mkSubWidget container $
                mkDriverIdWindow (not <$> bDriverIdWinHidden) (pure False)
                (i ^. trainDriverID)

    return (mainWinW, hideDriverIdWin)





