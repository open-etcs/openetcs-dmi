{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Control.Concurrent
import           Control.Monad
import           ETCS.DMI
import           ETCS.DMI.Types
import           GHCJS.DOM                  (enableInspector, runWebGUI,
                                             webViewGetDomDocument)
import           GHCJS.DOM.Document         (getElementById)
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.Frameworks

trainb :: TrainBehavior
trainb =
  TrainBehavior {
    _trainIsAtStandstill = pure True,
    _trainMode = pure SH,
    _trainLevel = pure Level2,
    _trainDriverIDIsValid = pure True,
    _trainDataIsValid = pure True,
    _trainLevelIsValid = pure True,
    _trainRunningNumberIsValid = pure True,
    _trainHasPendingEmergencyStop = pure False,
    _trainHasCommunicationSession = pure True,
    _trainIsNonLeading = pure False,
    _trainIsPassiveShunting  = pure False,
    _trainModDriverIDAllowed = pure True
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


