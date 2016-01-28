{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TypeFamilies          #-}

module ETCS.Binding.Raildriver where

import           Control.Lens                      hiding ((*~))
import           Data.Aeson
import           Data.Map
import qualified Data.Map                          as Map
import           Data.Set                          (Set)
import           Data.Text                         (Text)
import           Data.Typeable
import           Data.Word
import           ETCS.Bindings
import           ETCS.DMI
import           GHC.Generics                      (Generic)
import           Numeric.Units.Dimensional.Prelude
import qualified Prelude                           ()
import           Reflex

data RdLokoInfo =
  RdLokoInfo {
  _rdLokoProducer :: Text,
  _rdLokoProduct  :: Text,
  _rdLokoModel    :: Text,
  _rdLokoMinMax   :: Map Text (Double, Double)
  } deriving (Eq, Show, Typeable, Generic)

makeLenses ''RdLokoInfo

instance ToJSON RdLokoInfo where
  toJSON = toJSON
  toEncoding = genericToEncoding etcsJSONEncoding

instance FromJSON RdLokoInfo where
  parseJSON = genericParseJSON etcsJSONEncoding

rdLokoKeys :: Getter RdLokoInfo (Set Text)
rdLokoKeys = to $ Map.keysSet . view rdLokoMinMax


data RdValuesUpdateServer =
  RdValuesUpdateServer {
  _rdValueUpdateServer :: Map Text Float
  } deriving (Eq, Show, Typeable, Generic)

makeLenses ''RdValuesUpdateServer

instance ToJSON RdValuesUpdateServer where
  toJSON = toJSON
  toEncoding = genericToEncoding etcsJSONEncoding

instance FromJSON RdValuesUpdateServer where
  parseJSON = genericParseJSON etcsJSONEncoding


data RdValuesUpdateClient =
  RdValuesUpdateClient {
  _rdValueUpdateClient :: Map Text Float
  } deriving (Eq, Show, Typeable, Generic)

makeLenses ''RdValuesUpdateClient

instance ToJSON RdValuesUpdateClient where
  toJSON = toJSON
  toEncoding = genericToEncoding etcsJSONEncoding

instance FromJSON RdValuesUpdateClient where
  parseJSON = genericParseJSON etcsJSONEncoding


data RdPacket
  = RdPacketLokoInfo RdLokoInfo
  | RdPacketUpdateServer RdValuesUpdateServer
  | RdPacketUpdateClient RdValuesUpdateClient
  deriving (Eq, Show, Typeable, Generic)

makePrisms ''RdPacket

instance ToJSON RdPacket where
  toJSON = toJSON
  toEncoding = genericToEncoding etcsJSONEncoding

instance FromJSON RdPacket where
  parseJSON = genericParseJSON etcsJSONEncoding




data RaildriverBinding


instance (Reflex t) => TrainBehaviorBinding t RaildriverBinding where
  data TrainBehaviorBindingConfig RaildriverBinding =
    RailDriverBinding {
     _rdHost :: String,
     _rdTcpPort :: Word16
     } deriving (Eq, Show)
  initBinding cfg = do

    return $ TrainBehavior {
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






p1 = RdPacketLokoInfo $ RdLokoInfo "pr1" "pro1" "loko 1" $
  Map.fromList [("value1", (12,23))]


p2 :: Maybe RdPacket
p2 = decode . encode $ p1


