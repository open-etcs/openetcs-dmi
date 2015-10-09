{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module ETCS.DMI.MenuWindow ( MenuWindow, mkMenuWindow ) where

import           Data.Text                  (Text)
import           ETCS.DMI.Button
import           ETCS.DMI.ButtonGroup
import           ETCS.DMI.Helpers
import           GHCJS.DOM.Element          (setClassName)
import           GHCJS.DOM.HTMLElement      (setHidden)
import           GHCJS.DOM.Node             (appendChild, setTextContent)
import           GHCJS.DOM.Types            (castToElement)
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.Frameworks


data WindowTitle = WindowTitle

instance IsWidget WindowTitle where
  data WidgetInput WindowTitle = MkWindowTitle (Behavior Text)
  mkWidgetIO parent (MkWindowTitle t) = do
    doc <- _getOwnerDocument parent

    -- the window title
    titleElem <- _createDivElement doc
    () <$ appendChild parent (pure titleElem)

    let titleWidget = do
          let setTitle = setTextContent titleElem . pure
          valueBLater t >>= liftIOLater . setTitle
          changes t >>= reactimate' . fmap (fmap setTitle)
          return WindowTitle

    return (titleWidget, castToElement titleElem)

newtype MenuWindow =
  MenuWindow { menuWindowEvent :: Event (WidgetEventType MenuWindow) }

mkMenuWindow :: Behavior Text -> Event Bool ->
                [WidgetEventType MenuWindow ->
                 WidgetInput (Button (WidgetEventType MenuWindow))]
                -> WidgetInput MenuWindow
mkMenuWindow = MkMenuWindow

instance IsEventWidget MenuWindow where
  type WidgetEventType MenuWindow = Int
  widgetEvent = menuWindowEvent

instance IsWidget MenuWindow where
  data WidgetInput MenuWindow = MkMenuWindow {
    _menuWindowTitle   :: Behavior Text,
    _menuWindowVisible :: Event Bool,
    _menuWindowButtons :: [ Int -> WidgetInput (Button Int) ]
  }

  mkWidgetIO parent i = do
    doc <- _getOwnerDocument parent

    -- the window
    win <- _createDivElement doc
    setClassName win ("MenuWindow" :: String)

    -- the close button
    closeContainer <- _createDivElement doc
    setClassName closeContainer ("MenuClose" :: String)

    () <$ appendChild win (pure closeContainer)

    -- append window
    () <$ appendChild parent (pure win)

    let windowWidget = do
          () <$ (mkWidget win . MkWindowTitle . _menuWindowTitle $ i)

          -- the button group
          bg <- mkWidget win . mkButtonGroup . _menuWindowButtons $ i

          -- the close button
          closeButton <- mkWidget closeContainer $
                         mkButton UpButton (pure "x") (pure True) ()
          let eCloseIntern =
                unionWith const (_menuWindowVisible i) . fmap (const False) $
                widgetEvent closeButton


          reactimate $ fmap (setHidden win . not) eCloseIntern
          return . MenuWindow . widgetEvent $ bg
    return (windowWidget, castToElement win)

