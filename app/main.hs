{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where


import           Data.Maybe
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           ETCS.DMI
import           FRP.Sodium
import           GHCJS.DOM
import           GHCJS.DOM.Document            hiding (click, error, setTitle)
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


    mw <- mkMainWindow doc body mempty

    _ <- sync $ listen (_menuWinE mw) print



    return ()


mkMainWindow :: (IsDocument d, IsNode p) => d -> p -> Event Bool -> IO MenuWindow
mkMainWindow doc parent visible =
  mkMenuWindow doc parent (pure "Main") visible
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
                -> Behavior Text -> Event Bool ->[(Behavior Text, Behavior Bool )]
                -> IO MenuWindow
mkMenuWindow doc parent bTitle eVisible bs = do
  w <- _mkWindow
  cTitle <- _mkWindowTitle w
  bs' <- _mkButtons w
  closeButton <- _mkCloseButton w
  _ <- appendChild parent (pure w)

  return MenuWindow {
    _menuWinE = foldl merge mempty . fmap _buttonE $ bs',
    _menuWinCleanup = do _ <- sequence . fmap _buttonCleanup $ bs'
                         cTitle
                         _buttonCleanup closeButton
                         return ()
    }
  where _mkWindow = do
          w <- fmap (castToHTMLDivElement . fromMaybe (error "unable to create div")) $
               createElement doc $ pure ("div" :: String)
          setClassName w ("MenuWindow" :: String)
          return w
        _mkWindowTitle w = do
          t <- fmap (castToHTMLDivElement . fromMaybe (error "unable to create div")) $
               createElement doc $ pure ("div" :: String)
          cTitle <- sync $ listen (value bTitle) $ setTextContent t . pure
          setClassName t ("MenuTitle" :: String)
          _ <- appendChild w (pure t)
          return cTitle
        _mkButtons w = do
            bsContainer <-
              fmap (castToHTMLDivElement . fromMaybe (error "unable to create div")) $
              createElement doc $ pure ("div" :: String)
            setClassName bsContainer ("MenuButtons" :: String)

            bs' <- sequence $ zipWith (\i f -> f i) [minBound .. maxBound]
                   [ mkButton doc bsContainer b | b <- bs]
            _ <- appendChild w (pure bsContainer)
            return bs'
        _mkCloseButton w = do
            closeContainer <-
              fmap (castToHTMLDivElement . fromMaybe (error "unable to create div")) $
              createElement doc $ pure ("div" :: String)
            setClassName closeContainer ("MenuClose" :: String)
            closeButton <- mkButton doc closeContainer (pure "X", pure True) ()

            let eIntern = eVisible `merge` (fmap (const False) $ _buttonE closeButton)
            _ <- sync $ listen eIntern $ \v -> do
              setHidden w (not v)

            _ <- appendChild w (pure closeContainer)
            return closeButton

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
      setTitle b t
      cs <- hasChildNodes b
      setTextContent sp . pure $ t
      c0 <- getFirstChild b

      case (cs, T.null t) of
        (False, False) -> do
          _ <- appendChild b $ pure sp ; return ()
        (False, True)  -> do
          _ <- appendChild b $ pure empty_div ; return ()
        (True, False) -> do
          _ <- removeChild b c0
          _ <- appendChild b $ pure sp ; return ()
        (True, True)  -> do
          _ <- removeChild b c0
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

