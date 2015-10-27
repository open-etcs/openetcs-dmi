{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Control.Concurrent
import           Control.Lens                         hiding ((*~))
import           Control.Monad
import           ETCS.DMI
import           ETCS.DMI.StartupSequence
import           ETCS.DMI.Types
import           ETCS.DMI.Widgets.SpeedDial
import           GHCJS.DOM                            (enableInspector,
                                                       runWebGUI,
                                                       webViewGetDomDocument)
import           GHCJS.DOM.Document                   (getElementById)
import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              ()
import           Reactive.Banana
import           Reactive.Banana.Frameworks


kmh :: (Fractional a) => Unit DVelocity a
kmh = kilo meter / hour


trainb :: TrainBehavior
trainb = TrainBehavior {
    _trainVelocity = pure (132 *~ kmh),
    _trainMode = pure FS,
    _trainNonLeadingInput = pure False,
    _trainLevel = pure (_UnknownData # ()),
    _trainDriverID = pure (_UnknownData # ()),
    _trainData = pure (_UnknownData # ()),
    _trainRunningNumber = pure (_UnknownData # ()),
    _trainEmergencyBreakActive = pure False,
    _trainServiceBreakActive = pure False,
    _trainIsNonLeading = pure False,
    _trainModDriverIDAllowed = pure True,
    _trainRadioSafeConnection = pure NoConnection,
    _trainCommunicationSessionPending = pure False,
    _trainPassiveShuntingInput = pure False,
    _trainSpeedDial = pure SpeedDial400,
    _trainSDMData = sdmd
    }

sdmd :: SDMData
sdmd = SDMData {
  _sdmVperm    = pure $ 160 *~ kmh,
  _sdmVrelease = pure $ Nothing,
  _sdmVtarget  = pure $ 80 *~ kmh,
  _sdmVwarn    = pure $ 165 *~ kmh,
  _sdmVsbi     = pure $ 170 *~ kmh,
  _sdmVindication = pure $ 85 *~ kmh,
  _sdmStatus   = pure TSM
}


main :: IO ()
main = runWebGUI $ \ webView -> do
    enableInspector webView
    Just doc <- webViewGetDomDocument webView
    Just dmiMain <- getElementById doc ("dmiMain" :: String)

    network <- compile $ do

      sd <- mkWidget dmiMain $ mkSpeedDial trainb
      windowMain <- mkWidget dmiMain $ mkStartupSequence trainb
--      let (eClose, eValue) = split . widgetEvent . fromWidgetInstance $ windowMain
--      _ <- execute $ fmap (const $ removeWidget windowMain) eClose
--      reactimate $ fmap print eValue


{-
      ovw <- mkOverrideWindow dmiMain (pure False)
      spw <- mkSpecialWindow dmiMain (pure False)
      sew <- mkSettingsWindow dmiMain (pure False)
      rcw <- mkRBCContactWindow dmiMain (pure False)
-}

      return ()

    actuate network
    print ("startup done" :: String)
    forever $ do
      threadDelay 10000000


