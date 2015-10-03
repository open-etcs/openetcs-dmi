{-# LANGUAGE OverloadedStrings #-}
module ETCS.DMI where

import           Data.Text  (Text)
import           FRP.Sodium

data Button t =
  Button {
    _buttonE       :: Event t,
    _buttonCleanup :: IO ()
    }


data WindowMenuButtonId =
  B1 | B2 | B3 | B4 | B5 | B6 | B7 | B8
  deriving (Show, Read, Eq, Ord, Enum, Bounded)



data MenuWindow =
  MenuWindow {
    _menuWinE       :: Event WindowMenuButtonId,
    _menuWinCleanup :: IO ()
    }




