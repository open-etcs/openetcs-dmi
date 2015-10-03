{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where

import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Text                     (Text)
import           ETCS.DMI
import           FRP.Sodium
import           GHCJS.DOM
import           GHCJS.DOM.Document            hiding (click, error)
import           GHCJS.DOM.Element             hiding (click, error)
import           GHCJS.DOM.EventTarget
import           GHCJS.DOM.EventTargetClosures
import           GHCJS.DOM.HTMLButtonElement   as Button
import           GHCJS.DOM.HTMLElement
import           GHCJS.DOM.Node
import           GHCJS.DOM.Types               hiding (Event, Text)

main :: IO ()
main = runWebGUI $ \ webView -> do
    enableInspector webView
    Just doc <- webViewGetDomDocument webView
    Just body <- getBody doc

    setInnerHTML body (Just ("<h1>Hello World</h1>" :: Text))



    mw <- mkMenuWindow doc body (pure "Main")
          [ (pure "Start", pure True)
          , (pure "Driver ID", pure True)
          , (pure "Train Data", pure True)
          , (pure "Level", pure True)
          , (pure "Train running Number", pure False)
          , (pure "Shunting", pure True)
          , (pure "Non-Leading", pure True)
          , (pure "Maintain Shunting", pure True)
          ]

    let e = _menuWinE mw

    _ <- sync $ listen e print



    return ()


mkMenuWindow :: (IsDocument d, IsNode p) => d -> p
                -> Behavior Text -> [(Behavior Text, Behavior Bool )]
                -> IO MenuWindow
mkMenuWindow doc parent title bs = do
  bs' <-
    sequence $ zipWith (\i f -> f i) [minBound .. maxBound]
    [mkButton doc parent b | b <- bs]

  return MenuWindow {
    _menuWinE = foldl merge mempty . fmap _buttonE $ bs',
    _menuWinCleanup = do _ <- sequence . fmap _buttonCleanup $ bs' ; return ()
    }

mkButton :: (IsDocument d, IsNode p) => d -> p -> (Behavior Text, Behavior Bool) -> e
            -> IO (Button e)
mkButton doc parent (bLabel, bEnabled) eValue = do

  b <- fmap (castToHTMLButtonElement . fromMaybe (error "unable to create button")) $
       createElement doc $ Just ("button" :: String)
  _ <- appendChild parent (pure b)

  (e, fe, cleanup) <- sync $ do
    cLabel   <- listen (value bLabel)   $ setTextContent b . Just
    cVisible <- listen (value bEnabled) $ Button.setDisabled b . not
    (e, fe) <- newEvent
    return (e, fe, cLabel >> cVisible)

  let listener :: MouseEvent -> IO ()
      listener _ = sync $ fe eValue
  eventListener <- eventListenerNew listener
  addEventListener b ("click" :: String) (Just eventListener) False

  return $ Button {
    _buttonE = e,
    _buttonCleanup = cleanup
    }


{-
  newButton <- castToHTMLElement <$> createElement doc (Just "button")
  setInnerText newButton $ Just "Button"
  appendChild parent (Just newButton)
  return newButton
-}
