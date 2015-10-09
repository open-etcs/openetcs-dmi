{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module ETCS.DMI.Window ( Window, mkWindow, MenuWindow ) where

import           Data.Text                  (Text)
import           Data.Typeable
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



type MenuWindow = Window ButtonGroup

newtype Window a =
  Window { windowEvent :: Event (Either () (WidgetEventType a)) }

mkWindow :: Behavior Text -> Behavior Bool -> WidgetInput a -> WidgetInput (Window a)
mkWindow = MkWindow

instance (Typeable a, IsEventWidget a) => IsEventWidget (Window a) where
  type WidgetEventType (Window a) = Either () (WidgetEventType a)
  widgetEvent = windowEvent

instance (Typeable a, IsEventWidget a) => IsWidget (Window a) where
  data WidgetInput (Window a) = MkWindow {
    _windowTitle   :: Behavior Text,
    _windowVisible :: Behavior Bool,
    _windowWidget  :: WidgetInput a
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

          () <$ (mkWidget win . MkWindowTitle . _windowTitle $ i)

          inner_widget <- mkWidget win $ _windowWidget i

          -- the close button
          closeButton <- mkWidget closeContainer $
                         mkButton UpButton (Just $ pure "x") (pure True) ()

          let setShown = setHidden win . not
          valueBLater (_windowVisible i) >>= liftIOLater . setShown
          changes (_windowVisible i) >>= reactimate' . fmap (fmap setShown)

          let outputC = Left <$> widgetEvent closeButton
              outputB = Right <$> widgetEvent inner_widget
              output  = whenE (_windowVisible i)$ unionWith const outputB outputC
          return . Window $ output
    return (windowWidget, castToElement win)

