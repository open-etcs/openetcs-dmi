module ETCS.DMI.Button (
    ButtonType (..), mkButton
) where

import           Control.Concurrent
import           Control.Monad
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           ETCS.DMI.Helpers
import           ETCS.DMI.Types
import           GHCJS.DOM.Element          (setAttribute, setClassName)

import           GHCJS.DOM.HTMLElement      (setTitle)
import           GHCJS.DOM.Node             (appendChild, setTextContent)
import           GHCJS.DOM.Types            (IsDocument, castToHTMLElement)
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.Frameworks


data ButtonState = ButtonDisabled | ButtonEnabled | ButtonPressed
  deriving (Eq, Ord, Show, Enum, Bounded)




buttonState :: Bool -> Bool -> ButtonState
buttonState False     _ = ButtonDisabled
buttonState True  False = ButtonEnabled
buttonState True  True  = ButtonPressed


mkButton :: (IsDocument d, IsNode p, Show e) => d -> p ->
            (ButtonType, Behavior Text, Behavior Bool) -> e ->
            IO (MomentIO (Event e))
mkButton doc parent (buttonType, bLabel, bEnabled) buttonEventValue = do
  button <- _createDivElement doc
  setAttribute button "data-role" "button"
  inner_div <- _createDivElement doc
  inner_span <- _createSpanElement doc
  empty_div <- _createDivElement doc
  setClassName empty_div "EmptyButton"
  () <$ appendChild inner_div (pure inner_span)
  () <$ appendChild button (pure inner_div)
  mv_thread <- newEmptyMVar

  return $ do
    -- react on label changes
    let setLabel t = do
          setTitle button t
          setTextContent inner_span . pure $ t
          _ <- appendChild parent . pure $
               if T.null t
               then castToHTMLElement empty_div
               else castToHTMLElement button
          return ()
    valueBLater bLabel >>= liftIOLater . setLabel
    changes bLabel >>= reactimate' . fmap (fmap setLabel)

    -- construct and react on button pressed behavior
    (eButtonPressed, fireButtonPressed) <- newEvent
    bButtonPressed <- stepper False eButtonPressed
    let bButtonState = buttonState <$> bEnabled <*> bButtonPressed
    let stateHandler = setAttribute button "data-state" . show
    valueBLater bButtonState >>= liftIOLater . stateHandler
    changes bButtonState >>= reactimate' . fmap (fmap stateHandler)

    -- handle clicks
    (eButtonClick, fireButton') <- newEvent
    let fireButtonEventValue = fireButton' buttonEventValue

    -- mouse out
    let bButtonNotDisabled = (/= ButtonDisabled) <$> bButtonState
    eMouseOut  <- whenE bButtonPressed <$> registerMouseOut button
    let mouseOutHandler () = do
          () <$ maybe (return ()) killThread <$> tryTakeMVar mv_thread
          fireButtonPressed False
    reactimate $ fmap mouseOutHandler eMouseOut

    -- mouse down
    eMouseDown <- whenE bButtonNotDisabled <$> registerMouseDown button
    let mouseDownHandler () =
          case buttonType of
            UpButton -> fireButtonPressed True
            DownButton -> do
              fireButtonEventValue
              forkIO (repeatAction mv_thread fireButtonEventValue)
                >>= putMVar mv_thread
            DelayButton ->
              forkIO (animateDelay mv_thread fireButtonPressed)
                >>= putMVar mv_thread
    reactimate $ fmap mouseDownHandler eMouseDown

    -- mouse up
    eMouseUp   <- registerMouseUp button
    let mouseUpHandler () = do
          fireButtonPressed False
          case buttonType of
            UpButton -> fireButtonEventValue
            DownButton ->
              () <$ maybe (return ()) killThread <$> tryTakeMVar mv_thread
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

