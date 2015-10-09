{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           ETCS.DMI
import           ETCS.DMI.Types
import           GHCJS.DOM                  (enableInspector, runWebGUI,
                                             webViewGetDomDocument)
import           GHCJS.DOM.Document         (getBody)
import           Reactive.Banana
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
    Just body <- getBody doc

--    ovw <- mkOverrideWindow doc body mempty
--    spw <- mkSpecialWindow doc body mempty
--    sew <- mkSettingsWindow doc body mempty
--          rcw <- mkRBCContactWindow doc body mempty

    network <- compile $ do
      windowMain <- mkMainWindow trainb body (pure True)
      reactimate $ fmap print (widgetEvent windowMain)

    actuate network
    print ("startup done" :: String)



