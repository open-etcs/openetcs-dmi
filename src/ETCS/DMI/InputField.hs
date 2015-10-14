{-# LANGUAGE TypeFamilies #-}

module ETCS.DMI.InputField (InputField (..), mkInputField) where

import           Control.Monad.Writer       (lift)
import           Data.Text                  (Text)
import           ETCS.DMI.Helpers
import           GHCJS.DOM.Element          (Element, setAttribute,
                                             setClassName)
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
  inputFieldGotFocus :: Event (),
  inputFieldRoot     :: Element
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
  widgetRoot = inputFieldRoot

  mkWidgetIO parent i = do
    doc   <- _getOwnerDocument parent
    field <- _createDivElement doc
    dataArea <- _createDivElement doc
    setTabIndex dataArea 1

    let setFieldTitle = setTitle field
    lift $ do
      valueBLater (_inputLabel i) >>= liftIOLater . setFieldTitle
      changes (_inputLabel i)  >>= reactimate' . fmap (fmap setFieldTitle)

    -- focus
    (eFocus, fireFocus) <- lift newEvent
    eHasFocus <- lift . accumE False $ fmap (const not) eFocus
    bHasFocus <- lift $ stepper False eHasFocus
    let eGotFocus = whenE (fmap not bHasFocus) eFocus

    -- handle Reset
    let eReset = whenE bHasFocus (_inputReset i)
    lift . reactimate $ fmap fireFocus eReset


    eClickData <- registerMouseClick dataArea
    lift . reactimate $ fmap fireFocus eClickData

    eValueOnClick' <- lift . execute $ fmap (const $ valueB (_inputBuffer i)) eClickData
    let eValueOnClick = whenE bHasFocus eValueOnClick'

    let bDisplay =
          switchB (_inputValue i) $
          fmap (\f -> if f then _inputBuffer i else _inputValue i) eHasFocus

    -- what shall be actualy displayed (internal buffer or value)
    let setDataValue = setTextContent dataArea . pure
    lift $ do
      valueBLater bDisplay >>= liftIOLater . setDataValue
      changes bDisplay  >>= reactimate' . fmap (fmap setDataValue)

    -- set state
    let _state True  False = Accepted
        _state _     True  = Selected
        _state False False = NotSelected
        bState = _state <$> _inputIsAccepted i <*> bHasFocus
    let setFieldState = setAttribute dataArea "data-state" . show
    lift $ do
      valueBLater bState >>= liftIOLater . setFieldState
      changes bState >>= reactimate' . fmap (fmap setFieldState)

    (eFieldEnter, fireFieldEnter) <- lift newEvent

    if _inputIsSingle i then do
      lift . liftIOLater $ do
        () <$ appendChild field (pure dataArea)
        () <$ appendChild parent (pure field)
        setClassName dataArea "DataAreaFull"
      return $ InputField eValueOnClick eGotFocus (castToElement field)
      else do
      labelArea <- liftIO $ _createDivElement doc
      lift . liftIOLater $ setClassName labelArea "LabelArea"
      let setLabel = setTextContent labelArea . pure
      lift $ do
        valueBLater (_inputLabel i) >>= liftIOLater . setLabel
        changes (_inputLabel i)  >>= reactimate' . fmap (fmap setLabel)
      eClickLabel <- registerMouseClick labelArea
      lift . reactimate . fmap fireFocus $ eClickLabel

      lift . liftIOLater $ do
        () <$ appendChild field (pure labelArea)
        () <$ appendChild field (pure dataArea)
        () <$ appendChild parent (pure field)
        setClassName dataArea "DataAreaLabel"

      return $ InputField eValueOnClick eGotFocus (castToElement field)

