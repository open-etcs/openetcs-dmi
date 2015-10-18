{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module ETCS.DMI.DataEntryWindow where



import           Control.Lens                         hiding ((*~))
import           Data.Proxy
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Typeable                        (Typeable)
import           ETCS.DMI.Helpers
import           ETCS.DMI.Widgets.Keyboard
import           GHCJS.DOM.Types                      (IsNode)
import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              ()
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.DOM.Widget
import           Reactive.Banana.Frameworks

class (Typeable (DataValueKeyboard a), IsEventWidget (DataValueKeyboard a)) =>
      DataValue a where
  type DataValueType a :: *
  type DataValueKeyboard a :: *
  type DataValueKeyboardType a :: *

  dataValueLabel :: Proxy a -> Behavior Text
  mkKeyboard :: Proxy a -> Behavior Bool -> WidgetInput (DataValueKeyboard a)
  fromKeyboardEvent :: (MonadMoment m) =>
                       Proxy a ->
                       Event r ->
                       Event (WidgetEventType (DataValueKeyboard a)) ->
                       m (Behavior Text)
  fromText :: Text -> a
  _DataValueKeyboard :: Prism' (DataValueKeyboardType a) a


mkDataValueKeyboard :: (DataValue a, IsNode n) => Proxy a -> Event r -> Behavior Bool -> n ->
         MomentIO (Behavior Text, Behavior Text, Proxy a)
mkDataValueKeyboard t r v parent = do
    kbd <- mkWidget parent $ mkKeyboard t v
    kbd_buf <- fromKeyboardEvent t r (widgetEvent . fromWidgetInstance $ kbd)
    return (dataValueLabel t, kbd_buf, t)


newtype MaxSpeed = MaxSpeed (Velocity Double)
makePrisms ''MaxSpeed

instance DataValue MaxSpeed where
  type DataValueType MaxSpeed = Velocity Double

  type DataValueKeyboard MaxSpeed = NumericKeyboard
  type DataValueKeyboardType MaxSpeed = Int

  dataValueLabel _ = pure "Max speed (km/h)"
  mkKeyboard _ = mkNumericKeyboard
  fromKeyboardEvent _ r e = fmap T.pack <$> mkKeyboardBuffer 16 r e
  fromText t = MaxSpeed $ (read . T.unpack $ t) *~ kmh
  _DataValueKeyboard = prism a b
    where a (MaxSpeed i) = round $ i /~ kmh
          b i = if i >= 0 then Right . MaxSpeed $ (fromIntegral i) *~ kmh else Left i


newtype TrainLength = TrainLength (Length Double)
instance DataValue TrainLength where
  type DataValueType TrainLength = Length Double
  type DataValueKeyboard TrainLength = NumericKeyboard
  dataValueLabel _ = pure "Length (m)"
  mkKeyboard _ = mkNumericKeyboard
  fromKeyboardEvent _ r e = fmap T.pack <$> mkKeyboardBuffer 16 r e
  fromText t = TrainLength $ (read . T.unpack $ t) *~ meter

newtype BreakPercentage = BreakPercentage (Dimensionless Double)
instance DataValue BreakPercentage where
  type DataValueType BreakPercentage = Dimensionless Double
  type DataValueKeyboard BreakPercentage = NumericKeyboard
  dataValueLabel _ = pure "Break percentage"
  mkKeyboard _ = mkNumericKeyboard
  fromKeyboardEvent _ r e = fmap T.pack <$> mkKeyboardBuffer 16 r e
  fromText t = BreakPercentage $ (read . T.unpack $ t) *~ one

data TrainCategoryType =
  PASS1 | PASS2 | PASS3 |
  TILT1 | TILT2 | TILT3 | TILT4 | TILT5 | TILT6 | TILT7 |
  FP1 | FP2 | FP3 | FP4 |
  FG1 | FG2 | FG3 | FG4
  deriving (Eq, Ord, Enum, Bounded, Show, Read)


newtype TrainCategory = TrainCategory (Maybe TrainCategoryType)

instance DataValue TrainCategory where
  type DataValueType TrainCategory = Maybe TrainCategoryType
  type DataValueKeyboard TrainCategory = DedicatedKeyboard TrainCategoryType
  dataValueLabel _ = pure "Break percentage"
  mkKeyboard _ = mkEnumKeyboard
  fromKeyboardEvent _ r e =
    stepper "" $ unionWith const
    (fmap (const "") r)
    (fmap (T.pack . show) e)
  fromText = TrainCategory . read . T.unpack



