{-# LANGUAGE TypeFamilies #-}

module ETCS.DMI.InputField where

import           Data.Text                  (Text)
import           ETCS.DMI.Helpers
import           GHCJS.DOM.Element          (setAttribute, setClassName)
import           GHCJS.DOM.HTMLElement      (setContentEditable, setTitle)
import           GHCJS.DOM.Node             (appendChild, setTextContent)
import           GHCJS.DOM.Types            (castToElement)
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.Frameworks



newtype InputField = InputField ()

mkInputField :: Behavior Text -> Bool -> WidgetInput InputField
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
    let fieldWidget = do
          let setFieldTitle = setTitle field
          valueBLater (_inputLabel i) >>= liftIOLater . setFieldTitle
          changes (_inputLabel i)  >>= reactimate' . fmap (fmap setFieldTitle)

          if _inputIsSingle i then do
            liftIOLater $ do
              () <$ appendChild field (pure dataArea)
              () <$ appendChild parent (pure field)
              setClassName dataArea "DataAreaFull"

            return $ InputField ()
          else do
            labelArea <- liftIO $ _createDivElement doc
            liftIOLater $ setClassName labelArea "LabelArea"
            let setLabel = setTextContent labelArea . pure
            valueBLater (_inputLabel i) >>= liftIOLater . setLabel
            changes (_inputLabel i)  >>= reactimate' . fmap (fmap setLabel)

            liftIOLater $ do
              () <$ appendChild field (pure labelArea)
              () <$ appendChild field (pure dataArea)
              () <$ appendChild parent (pure field)
              setClassName dataArea "DataAreaLabel"

            return $ InputField ()

    return (fieldWidget, castToElement field)
