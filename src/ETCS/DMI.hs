{-# LANGUAGE OverloadedStrings #-}

module ETCS.DMI
       ( mkMainWindow, menuWinE, menuWinCleanup
       ) where

import           ETCS.DMI.MenuWindow
import           FRP.Sodium
import           GHCJS.DOM.Types     (IsDocument, IsNode)

mkMainWindow :: (IsDocument d, IsNode p) => d -> p -> Event Bool -> IO MenuWindow
mkMainWindow doc parent visible =
  mkMenuWindow doc parent (pure "Main") visible
  [ (pure "Start", pure True)
  , (pure "Driver ID", pure True)
  , (pure "Train Data", pure True)
  , (pure "", pure False)
  , (pure "Level", pure True)
  , (pure "Train running Number", pure False)
  , (pure "Shunting", pure True)
  , (pure "Non-Leading", pure True)
  , (pure "Maintain Shunting", pure True)
  ]
