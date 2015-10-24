{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module ETCS.DMI.Windows.DriverIdWindow (DriverIdWindow, mkDriverIdWindow) where


import           Control.Lens
import           Control.Monad.Writer
import           Data.Text                    (Text)
import           ETCS.DMI.Helpers
import           ETCS.DMI.TrainBehavior
import           ETCS.DMI.Types
import           ETCS.DMI.Widgets.Button
import           ETCS.DMI.Widgets.ButtonGroup
import           ETCS.DMI.Widgets.InputField
import           ETCS.DMI.Widgets.Keyboard
import           ETCS.DMI.Windows.Window
import           GHCJS.DOM.Node               (appendChild)
import           GHCJS.DOM.Types              (castToElement)
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.DOM.Widget
import           Reactive.Banana.Frameworks

data DriverIdSubWidget =
  DriverIdSubWidget {
    driverIdSubWidgetEvent :: Event DriverId
    }


instance IsWidget DriverIdSubWidget where
  data WidgetInput DriverIdSubWidget = MkDriverIdSubWidget {
    _driverIdSubWidgetWindowInput :: WidgetInput DriverIdWindow
    }

  mkWidgetInstance parent i = do
    doc <- _getOwnerDocument parent
    container <- _createDivElement doc


    let onboard = _driverIdWindowDataIn . _driverIdSubWidgetWindowInput $ i

    (buffer, changeBuffer) <- lift . newBehavior $ mempty
    (reset, fireReset) <- lift $ newEvent

    field <-
      mkSubWidget container $ mkInputField (pure mempty) True
      (onBoardDataWithDefault mempty <$> onboard)
      (onBoardIsValid <$> onboard) buffer reset

    kbd <- mkSubWidget container $ mkAlphaNumKeyboard  (pure True)



    () <$ appendChild parent (pure container)
    return (DriverIdSubWidget never, castToElement container)

instance IsEventWidget DriverIdSubWidget where
  type WidgetEventType DriverIdSubWidget =  DriverId
  widgetEvent = driverIdSubWidgetEvent


data DriverIdWindow =
  DriverIdWindow {
    driverIdWindowEvent :: Event DriverId
    }

mkDriverIdWindow = MkDriverIdWindow

instance IsWidget DriverIdWindow where
  data WidgetInput DriverIdWindow = MkDriverIdWindow {
    _driverIdWindowVisible :: Behavior Bool,
    _driverIdWindowHideCloseButton :: Behavior Bool,
    _driverIdWindowDataIn :: Behavior DriverIdData
    }

  mkWidgetInstance parent i =
    let mkWin = mkSubWidget parent $ mkWindow (pure "Driver ID") (pure Nothing)
                (_driverIdWindowVisible i) (_driverIdWindowHideCloseButton i)
                (MkDriverIdSubWidget i)
    in do
      w <- mkWin
      let wroot = widgetRoot w

      let e = snd . split <$> widgetEvent . fromWidgetInstance $ w

      return (DriverIdWindow e, wroot)



