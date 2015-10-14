{-# LANGUAGE TypeFamilies #-}

module ETCS.DMI.Button (
    Button, ButtonType (..), mkButton
) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Writer
import           Data.Text                  (Text)
import           ETCS.DMI.Helpers
import           ETCS.DMI.Types
import           GHCJS.DOM.Element          (Element, setAttribute,
                                             setClassName)
import           GHCJS.DOM.HTMLElement      (setTitle)
import           GHCJS.DOM.Node             (appendChild, setTextContent)
import           GHCJS.DOM.Types            (castToElement, castToHTMLElement)
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.Frameworks


data ButtonState = ButtonDisabled | ButtonEnabled | ButtonPressed
  deriving (Eq, Ord, Show, Enum, Bounded)

mkButton :: ButtonType -> Maybe (Behavior Text) -> Behavior Bool -> e -> WidgetInput (Button e)
mkButton = MkButton

data Button e =
  Button { buttonEvent :: Event e, buttonRoot :: Element }

instance IsEventWidget (Button e) where
  type WidgetEventType (Button e) = e
  widgetEvent = buttonEvent

instance IsWidget (Button e) where
  data WidgetInput (Button e) = MkButton {
    _buttonType :: ButtonType,
    _buttonText :: Maybe (Behavior Text),
    _buttonEnabled :: Behavior Bool,
    _buttonValue :: e
    }
  widgetRoot = buttonRoot
  mkWidgetIO parent i = do
    doc        <- _getOwnerDocument parent
    button     <- _createDivElement doc
    inner_div  <- _createDivElement doc
    inner_span <- _createSpanElement doc
    empty_div  <- _createDivElement doc

    lift $ liftIOLater $ do
      setClassName empty_div "EmptyButton"
      () <$ appendChild inner_div (pure inner_span)
      () <$ appendChild button (pure inner_div)

    case _buttonText i of
      Nothing -> do
        _ <- appendChild parent . pure $ castToHTMLElement empty_div
        return $ Button never (castToElement button)
      Just t -> do
        mv_thread <- liftIO newEmptyMVar
        let killThreadIfExists :: IO ()
            killThreadIfExists = tryTakeMVar mv_thread >>= maybe (return ()) killThread
        registerCleanupIO killThreadIfExists

        _ <- appendChild parent . pure $ castToHTMLElement button
        let setLabel l = do
              setTitle button l
              setTextContent inner_span . pure $ l
        lift $ valueBLater t >>= liftIOLater . setLabel
        lift $ changes t >>= reactimate' . fmap (fmap setLabel)

        -- construct and react on button pressed behavior
        (eButtonPressed, fireButtonPressed) <- lift newEvent
        bButtonPressed <- lift $ stepper False eButtonPressed
        let bButtonState = buttonState <$> _buttonEnabled i <*> bButtonPressed
        let stateHandler = setAttribute button "data-state" . show
        lift $ valueBLater bButtonState >>= liftIOLater . stateHandler
        lift $ changes bButtonState >>= reactimate' . fmap (fmap stateHandler)
        let bButtonNotDisabled = (/= ButtonDisabled) <$> bButtonState

        -- internal click event
        (eButtonClick, fireButton') <- lift newEvent
        let fireButtonEventValue = fireButton' (_buttonValue i)

        -- mouse out
        eMouseOut' <- registerMouseOut button
        let eMouseOut = whenE (bOr bButtonNotDisabled bButtonPressed) eMouseOut'

        let mouseOutHandler () = do
              () <$ killThreadIfExists
              fireButtonPressed False
        lift . reactimate $ fmap mouseOutHandler eMouseOut

        -- mouse down
        eMouseDown' <- registerMouseDown button
        let eMouseDown = whenE bButtonNotDisabled eMouseDown'

        let mouseDownHandler () = case _buttonType i of
              UpButton -> fireButtonPressed True
              DownButton -> do
                fireButtonEventValue
                fireButtonPressed True
                forkIO (repeatAction mv_thread fireButtonEventValue) >>=
                  putMVar mv_thread
              DelayButton ->
                forkIO (animateDelay mv_thread fireButtonPressed) >>=
                  putMVar mv_thread
        lift . reactimate $ fmap mouseDownHandler eMouseDown


        -- mouse up
        eMouseUp' <- registerMouseUp button
        let eMouseUp =  whenE bButtonNotDisabled eMouseUp'

        let mouseUpHandler () = do
              fireButtonPressed False
              case _buttonType i of
                UpButton -> fireButtonEventValue
                DownButton -> () <$ killThreadIfExists
                DelayButton ->
                  tryTakeMVar mv_thread >>= maybe fireButtonEventValue killThread
        lift . reactimate $ fmap mouseUpHandler eMouseUp

        return $ Button eButtonClick (castToElement button)



-- buttonEnabled -> buttonState  -> ButtonState
buttonState :: Bool -> Bool -> ButtonState
buttonState False     _ = ButtonDisabled
buttonState True  False = ButtonEnabled
buttonState True  True  = ButtonPressed

-- used by down down
repeatAction :: MVar a -> IO () -> IO ()
repeatAction mv_thread fireButtonEventValue = do
  threadDelay 1500000
  em <- isEmptyMVar mv_thread
  unless em repeatAction'
  where repeatAction' = do
          em <- isEmptyMVar mv_thread
          unless em $ do fireButtonEventValue
                         threadDelay 300000
                         repeatAction'
-- used by delay button
animateDelay :: MVar a -> (Bool -> IO ()) -> IO ()
animateDelay mv_thread fireButtonPressed = animateDelay' (8 :: Int) True
  where animateDelay' 0 v = do
          fireButtonPressed v
          _<- tryTakeMVar mv_thread
          return ()
        animateDelay' i v = do
          em <- isEmptyMVar mv_thread
          unless em $ do fireButtonPressed v
                         threadDelay 250000
                         animateDelay' (i - 1) (not v)

