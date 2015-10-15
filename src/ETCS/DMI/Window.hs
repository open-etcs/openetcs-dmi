{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module ETCS.DMI.Window ( Window, mkWindow, MenuWindow ) where

import           Control.Monad.Writer       (lift)
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
import           Reactive.Banana.DOM.Widget
import           Reactive.Banana.Frameworks

data WindowTitle = WindowTitle

instance IsWidget WindowTitle where
  data WidgetInput WindowTitle = MkWindowTitle (Behavior Text)

  mkWidgetInstance parent (MkWindowTitle t) = do
    doc <- _getOwnerDocument parent

    -- the window title
    titleElem <- _createDivElement doc
    () <$ appendChild parent (pure titleElem)

    let setTitle = setTextContent titleElem . pure
    lift $ do
      valueBLater t >>= liftIOLater . setTitle
      changes t >>= reactimate' . fmap (fmap setTitle)

    return (WindowTitle, castToElement titleElem)



type MenuWindow = Window ButtonGroup

data Window a =
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


  mkWidgetInstance parent i = do
    doc <- _getOwnerDocument parent
    win <- _createDivElement doc
    closeContainer <- _createDivElement doc


    liftIO $ do
      setClassName closeContainer ("MenuClose" :: String)
      setClassName win ("MenuWindow" :: String)
      () <$ appendChild parent (pure win)
      () <$ appendChild win (pure closeContainer)

    _ <- mkSubWidget win . MkWindowTitle . _windowTitle $ i

    inner_widget <- mkSubWidget win $ _windowWidget i

    -- the close button
    closeButton <- mkSubWidget closeContainer $
                   mkButton UpButton (Just $ pure "x") (pure True) ()

    let setShown = setHidden win . not
    lift $ do
      valueBLater (_windowVisible i) >>= liftIOLater . setShown
      changes (_windowVisible i) >>= reactimate' . fmap (fmap setShown)

    let outputC = Left  <$> widgetEvent (fromWidgetInstance closeButton)
        outputB = Right <$> widgetEvent (fromWidgetInstance inner_widget)
        output  = whenE (_windowVisible i)$ unionWith const outputB outputC
    return (Window output, castToElement win)



