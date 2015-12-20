{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}

module Main ( main ) where

#if __GHCJS__

import           Control.Lens                      hiding ((*~))
import           Control.Monad
import           Control.Monad.IO.Class
import           ETCS.DMI
import           ETCS.DMI.Widgets.SpeedDial
import           GHCJS.DOM.Document                (querySelector)
import           GHCJS.DOM.Element                 hiding (error, querySelector)
import           GHCJS.DOM.Types
import           Numeric.Units.Dimensional.Prelude
import qualified Prelude                           as P
import           Reflex.Dom

trainb :: (Reflex t) => TrainBehavior t
trainb = TrainBehavior {
    _trainVelocity = constDyn (162.8 *~ kmh),
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


updateScaling :: (MonadIO m, IsDocument doc, IsElement el) =>
                 doc -> el -> m ()
updateScaling doc el' = do
  let el = castToElement el'
  w <- getClientWidth el
  h <- getClientHeight el
  let f = min ((P./) w 640) ((P./) h 480)
  m <- querySelector doc ("meta[name=viewport]" :: String)
  case m of
    Nothing -> fail "updateScaling: unable to find meta[name=viewport] in document."
    Just m -> do
      setAttribute m ("content" :: String) $ mconcat
        ["width=", show w, ", height=", show h,  ", initial-scale=", show f]
      return ()

main :: IO ()
main = mainWidget $ do
  doc <- askDocument
  (resizeE, outerDiv) <- resizeDetectorWithStyle "width: 100%; height: 100%;" $ do
    speedDial trainb
    askParent

  let outerDiv' = castToElement outerDiv
  updateScaling doc outerDiv'
  performEvent_ $ (const $ updateScaling doc outerDiv') <$> resizeE

  return ()

#else

main :: IO ()
main = return ()

#endif

