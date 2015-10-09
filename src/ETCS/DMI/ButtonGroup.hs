{-# LANGUAGE TypeFamilies #-}

module ETCS.DMI.ButtonGroup (ButtonGroup, mkButtonGroup) where


import           Control.Monad
import           ETCS.DMI.Button
import           ETCS.DMI.Helpers
import           GHCJS.DOM.Element   (setClassName)
import           GHCJS.DOM.Node      (appendChild)
import           Reactive.Banana
import           Reactive.Banana.DOM

newtype ButtonGroup =
  ButtonGroup {  buttonGroupEvent :: Event (WidgetEventType ButtonGroup) }

mkButtonGroup :: [Int -> WidgetInput (Button Int)] -> WidgetInput ButtonGroup
mkButtonGroup = MkButtonGroup

instance IsEventWidget ButtonGroup where
  type WidgetEventType ButtonGroup = Int
  widgetEvent = buttonGroupEvent

instance IsWidget ButtonGroup where
  data WidgetInput ButtonGroup = MkButtonGroup {
    _buttonGroupButtons :: [ WidgetEventType ButtonGroup ->
                             WidgetInput (Button (WidgetEventType ButtonGroup)) ]
  }

  mkWidgetIO parent i = do
    doc <- _getOwnerDocument parent
    bsContainer <- _createDivElement doc
    setClassName bsContainer ("MenuButtons" :: String)
    buttonsR <- zipWithM (\j f -> f j) [0 .. 9]
                [ mkWidgetIO bsContainer . b | b <- _buttonGroupButtons i]
    () <$ appendChild parent (pure bsContainer)

    return $ do
      -- union of all button events
      e <- foldl (unionWith const) never <$> sequence (fmap (fmap widgetEvent) buttonsR)
      return . ButtonGroup $ e
