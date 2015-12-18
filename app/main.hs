{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

#if __GHCJS__

import           Control.Lens                      hiding ((*~))
import           Control.Monad
import           ETCS.DMI
import           ETCS.DMI.Widgets.SpeedDial
import           Numeric.Units.Dimensional.Prelude
import           Prelude                           ()
import           Reflex.Dom

trainb :: (Reflex t) => TrainBehavior t
trainb = TrainBehavior {
    _trainVelocity = constDyn (132 *~ kmh),
    _trainMode = constDyn FS,
    _trainNonLeadingInput = constDyn False,
    _trainLevel = constDyn (_UnknownData # ()),
    _trainDriverID = constDyn (_UnknownData # ()),
    _trainData = constDyn (_UnknownData # ()),
    _trainRunningNumber = constDyn (_UnknownData # ()),
    _trainEmergencyBreakActive = constDyn False,
    _trainServiceBreakActive = constDyn False,
    _trainIsNonLeading = constDyn False,
    _trainModDriverIDAllowed = constDyn True,
    _trainRadioSafeConnection = constDyn NoConnection,
    _trainCommunicationSessionPending = constDyn False,
    _trainPassiveShuntingInput = constDyn False,
    _trainSpeedDial = constDyn SpeedDial400,
    _trainSdmData = sdmd
    }

sdmd :: (Reflex t) => SdmData t
sdmd = SdmData {
  _sdmVperm    = constDyn $ 160 *~ kmh,
  _sdmVrelease = constDyn $ Nothing,
  _sdmVtarget  = constDyn $ 80 *~ kmh,
  _sdmVwarn    = constDyn $ 165 *~ kmh,
  _sdmVsbi     = constDyn $ 170 *~ kmh,
  _sdmVindication = constDyn $ 85 *~ kmh,
  _sdmStatus   = constDyn TSM
}


main :: IO ()
main = mainWidget $ do
    sd <- speedDial trainb
    return ()


#else

main :: IO ()
main = return ()

#endif


{-
import           ETCS.DMI
import           ETCS.DMI.StartupSequence
import           ETCS.DMI.Widgets.SpeedDial
import           GHCJS.DOM                            (enableInspector,
                                                       runWebGUI,
                                                       webViewGetDomDocument)
import           GHCJS.DOM.Document                   (getElementById)
-}



{-
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


-}
