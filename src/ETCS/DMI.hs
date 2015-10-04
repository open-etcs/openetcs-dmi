{-# LANGUAGE OverloadedStrings #-}

module ETCS.DMI
       ( mkMainWindow, menuWinE, menuWinCleanup
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
