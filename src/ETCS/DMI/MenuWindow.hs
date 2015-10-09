{-# LANGUAGE OverloadedStrings #-}

module ETCS.DMI.MenuWindow ( mkMenuWindow ) where

import           Control.Monad
import           Data.Text                  (Text)
import           ETCS.DMI.Button
import           ETCS.DMI.ButtonGroup
import           ETCS.DMI.Helpers
import           GHCJS.DOM.Element          (setClassName)
import           GHCJS.DOM.HTMLElement      (setHidden)
import           GHCJS.DOM.Node             (appendChild, setTextContent)
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.Frameworks



mkMenuWindow :: (MonadIO m, IsNode p) => p
                -> Behavior Text -> Event Bool ->
                [ Int -> WidgetInput (Button Int) ]
                -> m (MomentIO (Event Int))
mkMenuWindow parent bTitle eVisible bs = do
  doc <- _getOwnerDocument parent

  -- the window
  win <- _createDivElement doc
  setClassName win ("MenuWindow" :: String)

  -- the window title
  titleElem <- _createDivElement doc
  setClassName titleElem ("MenuTitle" :: String)
  () <$ appendChild win (pure titleElem)

  -- the close button
  closeContainer <- _createDivElement doc
  setClassName closeContainer ("MenuClose" :: String)
  closeButtonR <-
    mkWidgetIO closeContainer $ mkButton UpButton (pure "x") (pure True) ()
  () <$ appendChild win (pure closeContainer)

  -- apend window
  () <$ appendChild parent (pure win)

  return $ do
    -- the title
    let titleHandler = setTextContent titleElem . pure
    valueBLater bTitle >>= liftIOLater . titleHandler
    changes bTitle >>= reactimate' . fmap (fmap titleHandler)

    -- the close button
    closeButton <- closeButtonR
    let eCloseIntern =
          unionWith const eVisible . fmap (const False) $ buttonEvent closeButton
    reactimate $ fmap (setHidden win . not) eCloseIntern

    -- the button group
    bg <- join . liftIO . mkWidgetIO win . mkButtonGroup $ bs
    return $ buttonGroupEvent bg
