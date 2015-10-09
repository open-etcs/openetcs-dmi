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

mkMenuWindow :: Behavior Text -> Behavior Bool ->
                [Int -> WidgetInput (Button Int)] -> WidgetInput MenuWindow
mkMenuWindow = MkMenuWindow

instance IsEventWidget MenuWindow where
  type WidgetEventType MenuWindow = Either () Int
  widgetEvent = menuWindowEvent

instance IsWidget MenuWindow where
  data WidgetInput MenuWindow = MkMenuWindow {
    _menuWindowTitle   :: Behavior Text,
    _menuWindowVisible :: Behavior Bool,
    _menuWindowButtons :: [ Int -> WidgetInput (Button Int) ]
  }

  mkWidgetIO parent i = do
    doc <- _getOwnerDocument parent
    win <- _createDivElement doc
    closeContainer <- _createDivElement doc

    let windowWidget = do
          liftIOLater $ do
            setClassName closeContainer ("MenuClose" :: String)
            setClassName win ("MenuWindow" :: String)
            () <$ appendChild parent (pure win)
            () <$ appendChild win (pure closeContainer)

          () <$ (mkWidget win . MkWindowTitle . _menuWindowTitle $ i)

          -- the button group
          bg <- mkWidget win . mkButtonGroup . _menuWindowButtons $ i

          -- the close button
          closeButton <- mkWidget closeContainer $
                         mkButton UpButton (pure "x") (pure True) ()

          let setShown = setHidden win . not
          valueBLater (_menuWindowVisible i) >>= liftIOLater . setShown
          changes (_menuWindowVisible i) >>= reactimate' . fmap (fmap setShown)

          let outputC = Left <$> widgetEvent closeButton
              outputB = Right <$> widgetEvent bg
              output  = unionWith const outputB outputC
          return . MenuWindow $ output
    return (windowWidget, castToElement win)

