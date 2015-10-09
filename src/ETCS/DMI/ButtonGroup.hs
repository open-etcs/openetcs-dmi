{-# LANGUAGE TypeFamilies #-}

module ETCS.DMI.ButtonGroup (ButtonGroup(..), mkButtonGroup) where


import           Control.Monad
import           ETCS.DMI.Button
import           ETCS.DMI.Helpers
import           GHCJS.DOM.Element   (setClassName)
import           GHCJS.DOM.Node      (appendChild)
import           Reactive.Banana
import           Reactive.Banana.DOM

newtype ButtonGroup =
  ButtonGroup {  buttonGroupEvent :: Event Int }

mkButtonGroup :: [Int -> WidgetInput (Button Int)] -> WidgetInput ButtonGroup
mkButtonGroup = MkButtonGroup

instance IsWidget ButtonGroup where
  data WidgetInput ButtonGroup = MkButtonGroup {
    _buttonGroupButtons :: [ Int -> WidgetInput (Button Int) ]
  }

  mkWidgetIO parent i = do
    doc <- _getOwnerDocument parent
    bsContainer <- _createDivElement doc
    setClassName bsContainer ("MenuButtons" :: String)
    buttonsR <- zipWithM (\j f -> f j) [(0 :: Int) .. 9]
                [ mkWidgetIO bsContainer . b | b <- _buttonGroupButtons i]
    () <$ appendChild parent (pure bsContainer)

    return $ do
      -- union of all button events
      e <- foldl (unionWith const) never <$> sequence (fmap (fmap buttonEvent) buttonsR)
      return . ButtonGroup $ e
