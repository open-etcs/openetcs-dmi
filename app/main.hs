
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

    mw <- mkMainWindow doc body mempty

    _ <- sync $ listen (mw ^. menuWinE) print

    return ()




