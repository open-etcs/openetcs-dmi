{-# LANGUAGE TypeFamilies #-}

module ETCS.DMI.ButtonGroup (ButtonGroup, mkButtonGroup) where


import           Control.Monad
import           ETCS.DMI.Button
import           ETCS.DMI.Helpers
import           GHCJS.DOM.Node             (appendChild)
import           GHCJS.DOM.Types            (castToElement)
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.Frameworks

newtype ButtonGroup =
  ButtonGroup {  buttonGroupEvent :: Event Int }

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

  mkWidgetIO parent i = do
    doc <- _getOwnerDocument parent
    bsContainer <- _createDivElement doc
    () <$ appendChild parent (pure bsContainer)

    let buttonGroupWidget :: MomentIO ButtonGroup
        buttonGroupWidget = do
          buttonsR <- zipWithM (\j f -> f j) [0 .. 9]
                      [ mkWidget bsContainer . b | b <- _buttonGroupButtons i]
          let e =  foldl (unionWith const) never . fmap widgetEvent $ buttonsR
          return $ ButtonGroup e
    return (buttonGroupWidget, castToElement bsContainer)
