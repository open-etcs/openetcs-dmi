{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}

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
    _trainVelocity = constDyn (162.3 *~ kmh),
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
    void $ speedDial trainb


#else

main :: IO ()
main = return ()

#endif

