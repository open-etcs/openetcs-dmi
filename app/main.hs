{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where


import           Control.Lens
import           ETCS.DMI
import           FRP.Sodium
import           GHCJS.DOM          (enableInspector, runWebGUI,
                                     webViewGetDomDocument)
import           GHCJS.DOM.Document (getBody)



main :: IO ()
main = runWebGUI $ \ webView -> do
    enableInspector webView
    Just doc <- webViewGetDomDocument webView
    Just body <- getBody doc

--    maw <- mkMainWindow doc body mempty
--    ovw <- mkOverrideWindow doc body mempty
--    spw <- mkSpecialWindow doc body mempty
--    sew <- mkSettingsWindow doc body mempty
    rcw <- mkRBCContactWindow doc body mempty
    _ <- sync $ listen (rcw ^. menuWinE) print

{-
    b <- mkButton doc body (pure "Test Button", pure True) (23 :: Int)
    _ <- sync $ listen (b ^. buttonE) print
-}
    return ()




