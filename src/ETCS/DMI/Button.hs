module ETCS.DMI.Button (
    Button, ButtonType (..), mkButton, buttonE, buttonCleanup
) where

import           Control.Concurrent
import           Control.Monad
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           ETCS.DMI.Helpers
import           ETCS.DMI.Types
import           GHCJS.DOM.Element             (setAttribute, setClassName)
import           GHCJS.DOM.EventTarget         (addEventListener)
import           GHCJS.DOM.EventTargetClosures (eventListenerNew)
import           GHCJS.DOM.HTMLElement         (setTitle)
import           GHCJS.DOM.Node                (appendChild, setTextContent)
import           GHCJS.DOM.Types               (IsDocument, IsNode, MouseEvent,
                                                castToHTMLElement)
import           Reactive.Banana
import           Reactive.Banana.Frameworks



data ButtonState = ButtonDisabled | ButtonEnabled | ButtonPressed
  deriving (Eq, Ord, Show, Enum, Bounded)

buttonState :: Bool -> Bool -> ButtonState
buttonState False     _ = ButtonDisabled
buttonState True  False = ButtonEnabled
buttonState True  True  = ButtonPressed

registerMouseDown, registerMouseUp, registerMouseOut ::
  (IsNode n ) => n -> IO (AddHandler ())
registerMouseDown = registerMouseEvent "mousedown"
registerMouseUp   = registerMouseEvent "mouseup"
registerMouseOut  = registerMouseEvent "mouseup"


registerMouseEvent :: (IsNode n) => String -> n -> IO (AddHandler ())
registerMouseEvent e t = do
  (addHandler, fire) <- newAddHandler
  let handler :: MouseEvent -> IO ()
      handler = const $ fire ()
  eventListener <- eventListenerNew handler
  addEventListener t e (pure eventListener) True
  return addHandler

mkButton :: (IsDocument d, IsNode p, Show e, Frameworks t) => d -> p ->
            (ButtonType, Behavior t Text, Behavior t Bool) -> e ->
            IO (Moment t (Event t e))
mkButton doc parent (buttonType, bLabel, bEnabled) buttonEventValue = do
  button <- _createDivElement doc
  setAttribute button "data-role" "button"
  inner_div <- _createDivElement doc
  inner_span <- _createSpanElement doc
  empty_div <- _createDivElement doc
  setClassName empty_div "EmptyButton"
  _ <- appendChild inner_div $ pure inner_span
  _ <- appendChild button $ pure inner_div

  mv_thread <- newEmptyMVar

  return $ do

    -- react on label changes
    let setLabel t = do
          setTitle button t
          setTextContent inner_span . pure $ t
          _removeFromParentIfExists parent button
          _removeFromParentIfExists parent empty_div
          _ <- appendChild parent . pure $
               if T.null t
               then castToHTMLElement empty_div
               else castToHTMLElement button
          return ()
    initial bLabel >>= liftIOLater . setLabel
    changes bLabel >>= reactimate' . fmap (fmap setLabel)

    -- construct and react on button pressed behavior
    (eButtonPressed, fireButtonPressed) <- newEvent
    let bButtonPressed = stepper False eButtonPressed
    let bButtonState = buttonState <$> bEnabled <*> bButtonPressed
    let stateHandler = setAttribute button "data-state" . show
    initial bButtonState >>= liftIOLater . stateHandler
    changes bButtonState >>= reactimate' . fmap (fmap stateHandler)

    -- handle clicks
    (eButtonClick, fireButton') <- newEvent
    let fireButtonEventValue = fireButton' buttonEventValue

    -- mouse out
    eMouseOut  <- liftIO (registerMouseOut button) >>= fromAddHandler
    let mouseOutHandler () = do
          _ <- maybe (return ()) killThread <$> tryTakeMVar mv_thread
          fireButtonPressed False
    reactimate $ fmap mouseOutHandler eMouseOut

    -- mouse down
    eMouseDown <- liftIO (registerMouseDown button) >>= fromAddHandler
    let mouseDownHandler () =
          case buttonType of
            UpButton -> fireButtonPressed True
            DownButton -> do
              fireButtonEventValue
              tid <- forkIO $ repeatAction mv_thread fireButtonEventValue
              putMVar mv_thread tid
            DelayButton -> do
              tid <- forkIO $ animateDelay mv_thread fireButtonPressed
              putMVar mv_thread tid
    reactimate $ fmap mouseDownHandler eMouseDown

    -- mouse up
    eMouseUp   <- liftIO (registerMouseUp button) >>= fromAddHandler
    let mouseUpHandler () = do
          fireButtonPressed False
          case buttonType of
            UpButton -> fireButtonEventValue
            DownButton -> do
              _ <- maybe (return ()) killThread <$> tryTakeMVar mv_thread
              return ()
            DelayButton ->
              tryTakeMVar mv_thread >>= maybe fireButtonEventValue killThread
    reactimate $ fmap mouseUpHandler eMouseUp

    return eButtonClick


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

