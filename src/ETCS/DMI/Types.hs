{-# LANGUAGE TemplateHaskell #-}

module ETCS.DMI.Types where

import           Control.Lens
import           FRP.Sodium

data Button t =
  Button {
    _buttonE       :: Event t,
    _buttonCleanup :: IO ()
    }

makeLenses ''Button
makePrisms ''Button

data WindowMenuButtonId =
  B1 | B2 | B3 | B4 | B5 | B6 | B7 | B8 | B9
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data MenuWindow =
  MenuWindow {
    _menuWinE       :: Event WindowMenuButtonId,
    _menuWinCleanup :: IO ()
    }

makeLenses ''MenuWindow
makePrisms ''MenuWindow
