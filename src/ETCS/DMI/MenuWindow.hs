{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module ETCS.DMI.MenuWindow ( MenuWindow(..), mkMenuWindow ) where

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


newtype MenuWindow =
  MenuWindow { menuWindowEvent :: Event Int }

mkMenuWindow :: Behavior Text -> Event Bool -> [Int -> WidgetInput (Button Int)]
                -> WidgetInput MenuWindow
mkMenuWindow = MkMenuWindow

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
      valueBLater (_menuWindowTitle i) >>= liftIOLater . titleHandler
      changes (_menuWindowTitle i) >>= reactimate' . fmap (fmap titleHandler)

      -- the close button
      closeButton <- closeButtonR
      let eCloseIntern =
            unionWith const (_menuWindowVisible i) . fmap (const False) $
              buttonEvent closeButton
      reactimate $ fmap (setHidden win . not) eCloseIntern

      -- the button group
      bg <- mkWidget win . mkButtonGroup . _menuWindowButtons $ i
      return . MenuWindow $ buttonGroupEvent bg


