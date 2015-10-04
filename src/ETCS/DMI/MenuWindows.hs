{-# LANGUAGE OverloadedStrings #-}

module ETCS.DMI.MenuWindows
       ( mkMainWindow, mkOverrideWindow, mkSpecialWindow, mkSettingsWindow
       , mkRBCContactWindow
       , menuWinE, menuWinCleanup
       ) where

import           ETCS.DMI.Button
import           ETCS.DMI.MenuWindow
import           FRP.Sodium
import           GHCJS.DOM.Types     (IsDocument, IsNode)


mkMainWindow :: (IsDocument d, IsNode p) => d -> p -> Event Bool -> IO MenuWindow
mkMainWindow doc parent visible =
  mkMenuWindow doc parent (pure "Main") visible
  [ (UpButton, pure "Start", pure True)
  , (UpButton, pure "Driver ID", pure True)
  , (UpButton, pure "Train Data", pure True)
  , (UpButton, pure "", pure False)
  , (UpButton, pure "Level", pure True)
  , (UpButton, pure "Train running Number", pure True)
  , (DelayButton, pure "Shunting", pure True)
  , (DelayButton, pure "Non-Leading", pure True)
  , (DelayButton, pure "Maintain Shunting", pure False)
  ]


mkOverrideWindow :: (IsDocument d, IsNode p) => d -> p -> Event Bool -> IO MenuWindow
mkOverrideWindow doc parent visible =
  mkMenuWindow doc parent (pure "Override") visible
  [ (UpButton, pure "EOA", pure True)
  ]


mkSpecialWindow :: (IsDocument d, IsNode p) => d -> p -> Event Bool -> IO MenuWindow
mkSpecialWindow doc parent visible =
  mkMenuWindow doc parent (pure "Special") visible
  [ (UpButton, pure "Ahension", pure True)
  , (UpButton, pure "SR speed / distance", pure True)
  , (DelayButton, pure "Train integrety", pure True)
  ]

mkSettingsWindow :: (IsDocument d, IsNode p) => d -> p -> Event Bool -> IO MenuWindow
mkSettingsWindow doc parent visible =
  mkMenuWindow doc parent (pure "Settings") visible
  [ (UpButton, pure "Language", pure True) -- TODO: Image SE03
  , (UpButton, pure "Volume", pure True) -- TODO: Image SE02
  , (UpButton, pure "Brightness", pure True) -- TODO: Image SE01
  , (UpButton, pure "System version", pure True)
  , (UpButton, pure "Set VBC", pure True)
  , (UpButton, pure "Remove VBC", pure True)
  ]

mkRBCContactWindow :: (IsDocument d, IsNode p) => d -> p -> Event Bool -> IO MenuWindow
mkRBCContactWindow doc parent visible =
  mkMenuWindow doc parent (pure "RBC Contact") visible
  [ (UpButton, pure "Contact last RBC", pure True)
  , (UpButton, pure "Use short number", pure True)
  , (UpButton, pure "Enter RBC data", pure True) -- TODO: Image SE01
  , (DelayButton, pure "Radio Network ID", pure True)
  ]
