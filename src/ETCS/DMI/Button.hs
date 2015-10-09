{-# LANGUAGE TypeFamilies #-}

module ETCS.DMI.Button (
    Button, ButtonType (..), mkButton
) where

import           Control.Concurrent
import           Control.Monad
import           Data.Text                  (Text)
import           ETCS.DMI.Helpers
import           ETCS.DMI.Types
import           GHCJS.DOM.Element          (setAttribute, setClassName)
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
  Button { buttonEvent :: Event e }

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
  mkWidgetIO parent i = do
    doc        <- _getOwnerDocument parent
    button     <- _createDivElement doc
    inner_div  <- _createDivElement doc
    inner_span <- _createSpanElement doc
    empty_div  <- _createDivElement doc
    mv_thread <- liftIO newEmptyMVar

    let   buttonWidget = do
            liftIOLater $ do
              setClassName empty_div "EmptyButton"
              () <$ appendChild inner_div (pure inner_span)
              () <$ appendChild button (pure inner_div)

            case _buttonText i of
              Nothing -> do
                _ <- appendChild parent . pure $ castToHTMLElement empty_div
                return ()
              Just t -> do
                _ <- appendChild parent . pure $ castToHTMLElement button
                let setLabel l = do
                      setTitle button l
                      setTextContent inner_span . pure $ l
                valueBLater t >>= liftIOLater . setLabel
                changes t >>= reactimate' . fmap (fmap setLabel)

            -- construct and react on button pressed behavior
            (eButtonPressed, fireButtonPressed) <- newEvent
            bButtonPressed <- stepper False eButtonPressed
            let bButtonState = buttonState <$> _buttonEnabled i <*> bButtonPressed
            let stateHandler = setAttribute button "data-state" . show
            valueBLater bButtonState >>= liftIOLater . stateHandler
            changes bButtonState >>= reactimate' . fmap (fmap stateHandler)
            let bButtonNotDisabled = (/= ButtonDisabled) <$> bButtonState

            -- internal click event
            (eButtonClick, fireButton') <- newEvent
            let fireButtonEventValue = fireButton' (_buttonValue i)

            -- mouse out
            eMouseOut  <- whenE (bOr bButtonNotDisabled bButtonPressed) <$>
                          registerMouseOut button
            let mouseOutHandler () = do
                  () <$ maybe (return ()) killThread <$> tryTakeMVar mv_thread
                  fireButtonPressed False
            reactimate $ fmap mouseOutHandler eMouseOut

            -- mouse down
            eMouseDown <- whenE bButtonNotDisabled <$> registerMouseDown button
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
            reactimate $ fmap mouseDownHandler eMouseDown


            -- mouse up
            eMouseUp <- whenE bButtonNotDisabled <$> registerMouseUp button
            let mouseUpHandler () = do
                  fireButtonPressed False
                  case _buttonType i of
                    UpButton -> fireButtonEventValue
                    DownButton ->
                      () <$ maybe (return ()) killThread <$> tryTakeMVar mv_thread
                    DelayButton ->
                      tryTakeMVar mv_thread >>= maybe fireButtonEventValue killThread
            reactimate $ fmap mouseUpHandler eMouseUp
            return . Button $ eButtonClick
    return (buttonWidget, castToElement button)



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

