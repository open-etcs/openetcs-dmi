{-# LANGUAGE TypeFamilies #-}

module ETCS.DMI.InputField where

import           Control.Concurrent
import           Control.Monad
import           Data.Text                  (Text)
import           ETCS.DMI.Helpers
import           ETCS.DMI.Types
import           GHCJS.DOM.Element          (setAttribute, setClassName)
import           GHCJS.DOM.HTMLElement      (setContentEditable, setTitle)
import           GHCJS.DOM.Node             (appendChild, setTextContent)
import           GHCJS.DOM.Types            (castToElement, castToHTMLElement)
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.Frameworks



newtype InputField = InputField ()

mkInputField = MkInputField

instance IsWidget (InputField) where
  data WidgetInput InputField = MkInputField {
    _inputLabel :: Behavior Text,
    _inputIsSingle :: Bool
    }

  mkWidgetIO parent i = do
    doc   <- _getOwnerDocument parent
    field <- _createDivElement doc
    dataArea <- _createDivElement doc
    setContentEditable dataArea $ pure "true"
    let fieldWidget =
          if _inputIsSingle i then do


            liftIOLater $ do
              () <$ appendChild field (pure dataArea)
              () <$ appendChild parent (pure field)
              setClassName dataArea "DataAreaFull"

            return $ InputField ()
          else do
            labelArea <- liftIO $ _createDivElement doc
            labelSpan <- liftIO $ _createSpanElement doc
            liftIOLater $ setClassName labelArea "LabelArea"
            let setLabel l = do
                  setTextContent labelSpan . pure $ l
            valueBLater (_inputLabel i) >>= liftIOLater . setLabel
            changes (_inputLabel i)  >>= reactimate' . fmap (fmap setLabel)

            liftIOLater $ do
              () <$ appendChild labelArea (pure labelSpan)
              () <$ appendChild field (pure labelArea)
              () <$ appendChild field (pure dataArea)
              () <$ appendChild parent (pure field)
              setClassName dataArea "DataAreaLabel"

            return $ InputField ()

    return (fieldWidget, castToElement field)
