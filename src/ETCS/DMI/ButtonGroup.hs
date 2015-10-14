{-# LANGUAGE TypeFamilies #-}

module ETCS.DMI.ButtonGroup (ButtonGroup, mkButtonGroup) where


import           Control.Monad
import           ETCS.DMI.Button
import           ETCS.DMI.Helpers
import           GHCJS.DOM.Element   (Element)
import           GHCJS.DOM.Node      (appendChild)
import           GHCJS.DOM.Types     (castToElement)
import           Reactive.Banana
import           Reactive.Banana.DOM


data ButtonGroup =
  ButtonGroup {  buttonGroupEvent :: Event Int, buttonGroupRoot :: Element }

mkButtonGroup :: [Int -> WidgetInput (Button Int)] -> WidgetInput ButtonGroup
mkButtonGroup = MkButtonGroup

instance IsEventWidget ButtonGroup where
  type WidgetEventType ButtonGroup = Int
  widgetEvent = buttonGroupEvent

instance IsWidget ButtonGroup where
  data WidgetInput ButtonGroup = MkButtonGroup {
    _buttonGroupButtons :: [ Int ->
                             WidgetInput (Button Int) ]
  }

  widgetRoot = buttonGroupRoot

  mkWidgetInstance parent i = do
    doc <- _getOwnerDocument parent
    bsContainer <- _createDivElement doc
    () <$ appendChild parent (pure bsContainer)

    buttonsR <-
      zipWithM ($) [ mkSubWidget bsContainer . b | b <- _buttonGroupButtons i] [0 .. 9]

    let e = foldl (unionWith const) never . fmap (widgetEvent . fromWidgetInstance) $
            buttonsR
    return $ ButtonGroup e (castToElement bsContainer)


