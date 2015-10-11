{-# LANGUAGE TypeFamilies #-}

module ETCS.DMI.InputField where

import           Data.Text                  (Text)
import           ETCS.DMI.Helpers
import           GHCJS.DOM.Element          (focus, setAttribute, setClassName)
import           GHCJS.DOM.HTMLElement      (setTabIndex, setTitle)
import           GHCJS.DOM.Node             (appendChild, setTextContent)
import           GHCJS.DOM.Types            (castToElement)
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.Frameworks



newtype InputField = InputField ()

mkInputField :: Behavior Text -> Bool -> Behavior Text -> Behavior Bool ->
                WidgetInput InputField
mkInputField = MkInputField

instance IsWidget (InputField) where
  data WidgetInput InputField = MkInputField {
    _inputLabel :: Behavior Text,
    _inputIsSingle :: Bool,
    _inputValue :: Behavior Text,
    _inputIsAccepted :: Behavior Bool
    }

  mkWidgetIO parent i = do
    doc   <- _getOwnerDocument parent
    field <- _createDivElement doc
    dataArea <- _createDivElement doc
    setTabIndex dataArea 1

    let fieldWidget = do
          let setFieldTitle = setTitle field
          valueBLater (_inputLabel i) >>= liftIOLater . setFieldTitle
          changes (_inputLabel i)  >>= reactimate' . fmap (fmap setFieldTitle)

          let setDataValue = setTextContent dataArea . pure
          valueBLater (_inputValue i) >>= liftIOLater . setDataValue
          changes (_inputValue i)  >>= reactimate' . fmap (fmap setDataValue)

          let setAccepted v =
                setAttribute dataArea "data-accepted" $ if v then "true" else "false"
          valueBLater (_inputIsAccepted i) >>= liftIOLater . setAccepted
          changes (_inputIsAccepted i)  >>= reactimate' . fmap (fmap setAccepted)

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

            lblClick <- registerMouseClick labelArea
            let lblClickHandler _ = focus dataArea
            reactimate $ fmap lblClickHandler lblClick

            liftIOLater $ do
              () <$ appendChild field (pure labelArea)
              () <$ appendChild field (pure dataArea)
              () <$ appendChild parent (pure field)
              setClassName dataArea "DataAreaLabel"

            return $ InputField ()

    return (fieldWidget, castToElement field)
