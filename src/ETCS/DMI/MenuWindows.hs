{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module ETCS.DMI.MenuWindows
       ( mkMainWindow, mkOverrideWindow, mkSpecialWindow, mkSettingsWindow
       , mkRBCContactWindow
       , module ETCS.DMI.Window
       ) where


import           ETCS.DMI.Button
import           ETCS.DMI.ButtonGroup
import           ETCS.DMI.MainWindow
import           ETCS.DMI.Window
import           GHCJS.DOM.Types            (IsNode)
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.DOM.Widget


mkOverrideWindow :: (IsNode p) => p -> Behavior Bool -> ReactiveDom (WidgetInstance MenuWindow)
mkOverrideWindow parent visible =
  mkSubWidget parent $ mkWindow (pure "Override") (pure Nothing) visible . mkButtonGroup $
  [ mkButton UpButton (pure $ pure "EOA") (pure True)
  ]


mkSpecialWindow :: (IsNode p) => p -> Behavior Bool -> ReactiveDom (WidgetInstance MenuWindow)
mkSpecialWindow parent visible =
  mkSubWidget parent $ mkWindow (pure "Special") (pure Nothing) visible . mkButtonGroup $
  [ mkButton UpButton (pure $ pure "Ahension") (pure True)
  , mkButton UpButton (pure $ pure "SR speed / distance") (pure True)
  , mkButton DelayButton (pure $ pure "Train integrety") (pure True)
  ]

mkSettingsWindow :: (IsNode p) => p -> Behavior Bool -> ReactiveDom (WidgetInstance MenuWindow)
mkSettingsWindow parent visible =
  mkSubWidget parent $ mkWindow (pure "Settings") (pure Nothing) visible . mkButtonGroup $
  [ mkButton UpButton (pure $ pure "Language") (pure True) -- TODO: Image SE03
  , mkButton UpButton (pure $ pure "Volume") (pure True) -- TODO: Image SE02
  , mkButton UpButton (pure $ pure "Brightness") (pure True) -- TODO: Image SE01
  , mkButton UpButton (pure $ pure "System version") (pure True)
  , mkButton UpButton (pure $ pure "Set VBC") (pure True)
  , mkButton UpButton (pure $ pure "Remove VBC") (pure True)
  ]

mkRBCContactWindow :: (IsNode p) => p -> Behavior Bool -> ReactiveDom (WidgetInstance MenuWindow)
mkRBCContactWindow parent visible =
  mkSubWidget parent $ mkWindow (pure "RBC Contact") (pure Nothing) visible . mkButtonGroup $
  [ mkButton UpButton (pure $ pure "Contact last RBC") (pure True)
  , mkButton UpButton (pure $ pure "Use short number") (pure True)
  , mkButton UpButton (pure $ pure "Enter RBC data") (pure True) -- TODO: Image SE01
  , mkButton DelayButton (pure $ pure "Radio Network ID") (pure True)
  ]
