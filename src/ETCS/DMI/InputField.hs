{-# LANGUAGE TypeFamilies #-}

module ETCS.DMI.InputField (InputField (..), mkInputField) where

import           Data.Text                  (Text)
import           ETCS.DMI.Helpers
import           GHCJS.DOM.Element          (setAttribute, setClassName)
import           GHCJS.DOM.HTMLElement      (setTabIndex, setTitle)
import           GHCJS.DOM.Node             (appendChild, setTextContent)
import           GHCJS.DOM.Types            (castToElement)
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.Frameworks


data InputFieldState = NotSelected | Selected | Accepted
                     deriving (Show, Eq, Ord, Bounded, Enum)


data InputField = InputField {
  inputFieldNewValue :: Event Text,
  inputFieldGotFocus :: Event ()
  }

mkInputField :: Behavior Text -> Bool -> Behavior Text -> Behavior Bool ->
                Behavior Text -> Event () -> WidgetInput InputField

mkInputField = MkInputField

instance IsWidget InputField where
  data WidgetInput InputField = MkInputField {
    _inputLabel :: Behavior Text,
    _inputIsSingle :: Bool,
    _inputValue :: Behavior Text,
    _inputIsAccepted :: Behavior Bool,
    _inputBuffer :: Behavior Text,
    _inputReset :: Event ()
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

          -- focus
          (eFocus, fireFocus) <- newEvent
          eHasFocus <- accumE False $ fmap (const not) eFocus
          bHasFocus <- stepper False eHasFocus
          let eGotFocus = whenE (fmap not bHasFocus) eFocus

          -- handle Reset
          let eReset = whenE bHasFocus (_inputReset i)
          reactimate $ fmap fireFocus eReset


          eClick <- registerMouseClick dataArea
          reactimate $ fmap (fireFocus) eClick

          eValueOnClick' <- execute $ fmap (const $ valueB (_inputBuffer i)) eClick
          let eValueOnClick = whenE bHasFocus eValueOnClick'

          let bDisplay =
                switchB (_inputValue i) $
                fmap (\f -> if f then _inputBuffer i else _inputValue i) eHasFocus

          -- what shall be actualy displayed (internal buffer or value)
          let setDataValue = setTextContent dataArea . pure
          valueBLater bDisplay >>= liftIOLater . setDataValue
          changes bDisplay  >>= reactimate' . fmap (fmap setDataValue)

          -- set state
          let _state True  False = Accepted
              _state _     True  = Selected
              _state False False = NotSelected
              bState = _state <$> _inputIsAccepted i <*> bHasFocus
          let setFieldState = setAttribute dataArea "data-state" . show
          valueBLater bState >>= liftIOLater . setFieldState
          changes bState >>= reactimate' . fmap (fmap setFieldState)

          (eFieldEnter, fireFieldEnter) <- newEvent

          if _inputIsSingle i then do
            liftIOLater $ do
              () <$ appendChild field (pure dataArea)
              () <$ appendChild parent (pure field)
              setClassName dataArea "DataAreaFull"
            return $ InputField eValueOnClick eGotFocus
          else do
            labelArea <- liftIO $ _createDivElement doc
            liftIOLater $ setClassName labelArea "LabelArea"
            let setLabel = setTextContent labelArea . pure
            valueBLater (_inputLabel i) >>= liftIOLater . setLabel
            changes (_inputLabel i)  >>= reactimate' . fmap (fmap setLabel)
            registerMouseClick labelArea >>= reactimate . fmap (fireFocus)


            liftIOLater $ do
              () <$ appendChild field (pure labelArea)
              () <$ appendChild field (pure dataArea)
              () <$ appendChild parent (pure field)
              setClassName dataArea "DataAreaLabel"

            return $ InputField eValueOnClick eGotFocus

    return (fieldWidget, castToElement field)
