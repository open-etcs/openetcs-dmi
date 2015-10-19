{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Control.Concurrent
import           Control.Lens                         hiding ((*~))
import           Control.Monad
import           ETCS.DMI
import           ETCS.DMI.Types
import           GHCJS.DOM                            (enableInspector,
                                                       runWebGUI,
                                                       webViewGetDomDocument)
import           GHCJS.DOM.Document                   (getElementById)
import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              ()
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.Frameworks



kmh :: (Fractional a) => Unit DVelocity a
kmh = kilo meter / hour


trainb :: TrainBehavior
trainb =
  TrainBehavior {
    _trainVelocity = pure (0 *~ kmh),
    _trainMode = pure SB,
    _trainLevel = pure (_InvalidData # Level0),
    _trainDriverID = pure (_InvalidData # mempty),
    _trainData = pure (_InvalidData # TrainData),
    _trainRunningNumber = pure (_InvalidData # 0),
    _trainEmergencyStop = pure False,
    _trainIsNonLeading = pure False,
    _trainModDriverIDAllowed = pure True,
    _trainRadioSafeConnection = pure NoConnection,
    _trainCommunicationSessionPending = pure False
    }


main :: IO ()
main = runWebGUI $ \ webView -> do
    enableInspector webView
    Just doc <- webViewGetDomDocument webView
    Just dmiMain <- getElementById doc ("dmiMain" :: String)

    network <- compile $ do
      windowMain <- mkWidget dmiMain $
                    mkMainWindow trainb (pure False) (pure True) (pure True)

      let (eClose, eValue) = split . widgetEvent . fromWidgetInstance $ windowMain
      _ <- execute $ fmap (const $ removeWidget windowMain) eClose
      reactimate $ fmap print eValue


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


