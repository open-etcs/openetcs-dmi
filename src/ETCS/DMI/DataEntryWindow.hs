{-# LANGUAGE OverloadedStrings #-}

module ETCS.DMI.DataEntryWindow where

import           Control.Lens
import           ETCS.DMI.Button
import           ETCS.DMI.ButtonGroup
import           ETCS.DMI.Helpers
import           ETCS.DMI.Types
import           ETCS.DMI.Window
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.Frameworks





type DataEnvtryValue = (Behavior Text, Behavior Text, WidgetInput w)


