{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module ETCS.DMI.Windows.MenuWindows
       ( mkMainWindow, mkOverrideWindow, mkSpecialWindow, mkSettingsWindow
       , mkRBCContactWindow
       , module ETCS.DMI.Windows.Window
       ) where


import           ETCS.DMI.Widgets.Button
import           ETCS.DMI.Widgets.ButtonGroup
import           ETCS.DMI.Windows.MainWindow
import           ETCS.DMI.Windows.Window
import           GHCJS.DOM.Types              (IsNode)
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.DOM.Widget


mkOverrideWindow :: (IsNode p) => p -> Behavior Bool -> ReactiveDom (WidgetInstance MenuWindow)
mkOverrideWindow parent visible =
  mkSubWidget parent $ mkWindow (pure "Override") (pure Nothing) visible (pure False) . mkButtonGroup  $
  [ mkButton UpButton (pure $ TextLabel "EOA") (pure True)
  ]


mkSpecialWindow :: (IsNode p) => p -> Behavior Bool -> ReactiveDom (WidgetInstance MenuWindow)
mkSpecialWindow parent visible =
  mkSubWidget parent $ mkWindow (pure "Special") (pure Nothing) visible (pure False) . mkButtonGroup $
  [ mkButton UpButton (pure $ TextLabel "Ahension") (pure True)
  , mkButton UpButton (pure $ TextLabel "SR speed / distance") (pure True)
  , mkButton DelayButton (pure $ TextLabel "Train integrety") (pure True)
  ]

mkSettingsWindow :: (IsNode p) => p -> Behavior Bool -> ReactiveDom (WidgetInstance MenuWindow)
mkSettingsWindow parent visible =
  mkSubWidget parent $ mkWindow (pure "Settings") (pure Nothing) visible (pure False) . mkButtonGroup $
  [ mkButton UpButton (pure $ TextLabel "Language") (pure True) -- TODO: Image SE03
  , mkButton UpButton (pure $ TextLabel "Volume") (pure True) -- TODO: Image SE02
  , mkButton UpButton (pure $ TextLabel "Brightness") (pure True) -- TODO: Image SE01
  , mkButton UpButton (pure $ TextLabel "System version") (pure True)
  , mkButton UpButton (pure $ TextLabel "Set VBC") (pure True)
  , mkButton UpButton (pure $ TextLabel "Remove VBC") (pure True)
  ]

mkRBCContactWindow :: (IsNode p) => p -> Behavior Bool -> ReactiveDom (WidgetInstance MenuWindow)
mkRBCContactWindow parent visible =
  mkSubWidget parent $ mkWindow (pure "RBC Contact") (pure Nothing) visible (pure False) . mkButtonGroup $
  [ mkButton UpButton (pure $ TextLabel "Contact last RBC") (pure True)
  , mkButton UpButton (pure $ TextLabel "Use short number") (pure True)
  , mkButton UpButton (pure $ TextLabel "Enter RBC data") (pure True) -- TODO: Image SE01
  , mkButton DelayButton (pure $ TextLabel "Radio Network ID") (pure True)
  ]
