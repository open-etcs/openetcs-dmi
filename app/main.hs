{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where

import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Text                     (Text)
import qualified Data.Text                     as T
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


    mw <- mkMainWindow doc body

    _ <- sync $ listen (_menuWinE mw) print
    _ <- sync $ listen (_menuWinCloseE mw) print



    return ()


mkMainWindow doc parent =
  mkMenuWindow doc parent (pure "Main")
  [ (pure "Start", pure True)
  , (pure "Driver ID", pure True)
  , (pure "Train Data", pure True)
  , (pure ".", pure False)
  , (pure "Level", pure True)
  , (pure "Train running Number", pure False)
  , (pure "Shunting", pure True)
  , (pure "Non-Leading", pure True)
  , (pure "Maintain Shunting", pure True)
  ]


mkMenuWindow :: (IsDocument d, IsNode p) => d -> p
                -> Behavior Text -> [(Behavior Text, Behavior Bool )]
                -> IO MenuWindow
mkMenuWindow doc parent bTitle bs = do

  -- the window element
  w <- fmap (castToHTMLDivElement . fromMaybe (error "unable to create div")) $
       createElement doc $ pure ("div" :: String)
  setClassName w ("MenuWindow" :: String)

  -- the window title
  t <- fmap (castToHTMLDivElement . fromMaybe (error "unable to create div")) $
       createElement doc $ pure ("div" :: String)
  cTitle <- sync $ listen (value bTitle) $ setTextContent t . pure
  setClassName t ("MenuTitle" :: String)
  _ <- appendChild w (pure t)



  -- the buttons
  bsContainer <-
    fmap (castToHTMLDivElement . fromMaybe (error "unable to create div")) $
    createElement doc $ pure ("div" :: String)
  setClassName bsContainer ("MenuButtons" :: String)

  bs' <- sequence $ zipWith (\i f -> f i) [minBound .. maxBound]
         [ mkButton doc bsContainer b | b <- bs]
  _ <- appendChild w (pure bsContainer)


  -- the close button
  closeContainer <-
    fmap (castToHTMLDivElement . fromMaybe (error "unable to create div")) $
    createElement doc $ pure ("div" :: String)
  setClassName closeContainer ("MenuClose" :: String)
  closeButton <- mkButton doc closeContainer (pure "X", pure True) ()
  _ <- appendChild w (pure closeContainer)

  -- add the thing to the parent
  _ <- appendChild parent (pure w)

  return MenuWindow {
    _menuWinE = foldl merge mempty . fmap _buttonE $ bs',
    _menuWinCloseE = _buttonE closeButton,
    _menuWinCleanup = do _ <- sequence . fmap _buttonCleanup $ bs'
                         cTitle
                         _buttonCleanup closeButton
                         return ()
    }

mkButton :: (IsDocument d, IsNode p) => d -> p -> (Behavior Text, Behavior Bool) -> e
            -> IO (Button e)
mkButton doc parent (bLabel, bEnabled) eValue = do

  b <- fmap (castToHTMLButtonElement . fromMaybe (error "unable to create button")) $
       createElement doc $ pure ("button" :: String)
  _ <- appendChild parent (pure b)

  sp <- fmap (castToHTMLSpanElement . fromMaybe (error "unable to span button")) $
       createElement doc $ pure ("span" :: String)
  empty_div <-
    fmap (castToHTMLDivElement . fromMaybe (error "unable to empty button div")) $
    createElement doc $ pure ("div" :: String)
  setClassName empty_div ("EmptyButton" :: String)

  (e, fe, cleanup) <- sync $ do
    cLabel <- listen (value bLabel) $ \t -> do
      cs <- hasChildNodes b
      setTextContent sp . pure $ t
      c0 <- getFirstChild b

      case (cs, T.null t) of
        (False, False) -> do
          _ <- appendChild b $ pure sp ; return ()
        (False, True)  -> do
          _ <- appendChild b $ pure empty_div ; return ()
        (True, False) -> do
          removeChild b c0
          _ <- appendChild b $ pure sp ; return ()
        (True, True)  -> do
          removeChild b c0
          _ <- appendChild b $ pure empty_div ; return ()



    cVisible <- listen (value bEnabled) $ Button.setDisabled b . not
    (e, fe) <- newEvent
    return (e, fe, cLabel >> cVisible)

  let listener :: MouseEvent -> IO ()
      listener _ = sync $ fe eValue
  eventListener <- eventListenerNew listener
  addEventListener b ("click" :: String) (pure eventListener) False

  return $ Button {
    _buttonE = e,
    _buttonCleanup = cleanup
    }

