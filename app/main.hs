{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}

module Main ( main ) where

#if __GHCJS__

import           Control.Lens                         hiding ((*~))
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Map                             as Map
import           ETCS.DMI
import           ETCS.DMI.Widgets.DistanceToTargetBar
import           ETCS.DMI.Widgets.SpeedDial
import           GHCJS.DOM.Document                   (querySelector)
import           GHCJS.DOM.Element                    hiding (error,
                                                       querySelector)
import           GHCJS.DOM.Types
import           Numeric.Units.Dimensional.Prelude
import qualified Prelude                              as P
import           Reflex.Dom



trainb :: (Reflex t) => TrainBehavior t
trainb = TrainBehavior {
  _trainMode = constDyn FS,
  _trainLevel = constDyn (_UnknownData # ()),
  _trainDriverID = constDyn (_UnknownData # ()),
  _trainData = constDyn (_UnknownData # ()),
  _trainRunningNumber = constDyn (_UnknownData # ()),
  _trainModDriverIDAllowed = constDyn True,
  _trainRadioSafeConnection = constDyn NoConnection,
  _trainCommunicationSessionPending = constDyn False,
  _trainSdmDynamic = SdmDynamic {
      _sdmVperm    = constDyn $ 160 *~ kmh,
      _sdmVrelease = constDyn $ Nothing,
      _sdmVtarget  = constDyn $ 80 *~ kmh,
      _sdmVwarn    = constDyn $ 165 *~ kmh,
      _sdmVsbi     = constDyn $ 170 *~ kmh,
      _sdmVindication = constDyn $ 85 *~ kmh,
      _sdmStatus   = constDyn TSM
      },
  _trainLocoDynamic = LokoDynamic {
      _lokoVelocity = constDyn (162.8 *~ kmh),
      _lokoNonLeadingInput      = constDyn False,
      _lokoEmergencyBreakActive  = constDyn False,
      _lokoServiceBreakActive   =  constDyn False,
      _lokoPassiveShuntingInput =  constDyn False,
      _lokoSpeedDial             = constDyn SpeedDial400
      }
  }


main :: IO ()
main = mainWidget $ do
  infs <- informationStatus trainb

  -- A/B/C/E
  buildElement "div" abceAttrs $ do
    -- A
    buildElement "div" aAttrs $ do

      -- A3
      distanceToTargetBar trainb infs

      return ()

    -- B
    buildElement "div" bAttrs $ do
      speedDial trainb infs
      return ()


    -- C
    buildElement "div" cAttrs $ do

      return ()

    -- E
    buildElement "div" eAttrs $ do

      return ()

    return ()

  return ()
    where
      abceAttrs :: AttributeMap
      abceAttrs = Map.fromList [ ("id", "ABCE") ]
      aAttrs :: AttributeMap
      aAttrs = Map.fromList [ ("id", "A") ]
      bAttrs :: AttributeMap
      bAttrs = Map.fromList [ ("id", "B") ]
      cAttrs :: AttributeMap
      cAttrs = Map.fromList [ ("id", "C") ]
      eAttrs :: AttributeMap
      eAttrs = Map.fromList [ ("id", "E") ]


#else

main :: IO ()
main = return ()

#endif

