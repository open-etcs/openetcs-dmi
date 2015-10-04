module ETCS.DMI.Button (
    Button, ButtonType (..), mkButton, buttonE, buttonCleanup
) where

import           Control.Concurrent
import           Control.Lens
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           ETCS.DMI.Helpers
import           ETCS.DMI.Types
import           FRP.Sodium
import           GHCJS.DOM.Element             (setAttribute, setClassName)
import           GHCJS.DOM.EventTarget         (addEventListener,
                                                removeEventListener)
import           GHCJS.DOM.EventTargetClosures (eventListenerNew)
import           GHCJS.DOM.HTMLElement         (setTitle)
import           GHCJS.DOM.Node                (appendChild, setTextContent)
import           GHCJS.DOM.Types               (IsDocument, IsNode, MouseEvent,
                                                castToHTMLElement)


data ButtonState = ButtonDisabled | ButtonEnabled | ButtonPressed
  deriving (Eq, Ord, Show, Enum, Bounded)

buttonState :: Bool -> Bool -> ButtonState
buttonState False     _ = ButtonDisabled
buttonState True  False = ButtonEnabled
buttonState True  True  = ButtonPressed


mkButton :: (IsDocument d, IsNode p, Show e) => d -> p ->
            (ButtonType, Behavior Text, Behavior Bool) -> e -> IO (Button e)
mkButton doc parent (buttonType, bLabel, bEnabled) buttonEventValue = do
  button <- _createDivElement doc
  setAttribute button "data-role" "button"
  inner_div <- _createDivElement doc
  inner_span <- _createSpanElement doc
  _ <- appendChild inner_div $ pure inner_span
  _ <- appendChild button $ pure inner_div

  (bButtonPressed, fireButtonPressed) <- sync $ newBehavior False

  mv_thread <- newEmptyMVar

  let mouseOutHandler :: MouseEvent -> IO ()
      mouseOutHandler _ = do
        _ <- maybe (return ()) killThread <$> tryTakeMVar mv_thread
        sync $ fireButtonPressed False

  eventListenerMouseOut <- eventListenerNew mouseOutHandler

  (eButtonClick, fireButtonClick) <- sync newEvent
  let fireButtonEventValue = sync $ fireButtonClick buttonEventValue

  let animateDelay' 0 v = do
        sync $ fireButtonPressed v
        _<- tryTakeMVar mv_thread
        return ()

      animateDelay' i v = do
        em <- isEmptyMVar mv_thread
        if (em) then return ()
          else do sync $ fireButtonPressed v
                  threadDelay 250000
                  animateDelay' (i - 1) (not v)
      animateDelay = animateDelay' (8 :: Int) True

      repeatAction' = do
        em <- isEmptyMVar mv_thread
        if (em) then return ()
          else do fireButtonEventValue
                  threadDelay 300000
                  repeatAction'
      repeatAction = do
        threadDelay 1500000
        em <- isEmptyMVar mv_thread
        if (em) then return ()
          else do repeatAction'


  let listenerDown, listenerUp :: MouseEvent -> IO ()
      listenerDown _ = do
        addEventListener button "mouseout" (pure eventListenerMouseOut) True
        case buttonType of
          UpButton -> sync $ fireButtonPressed True
          DownButton -> do
            fireButtonEventValue
            tid <- forkIO repeatAction
            putMVar mv_thread tid
          DelayButton -> do
            tid <- forkIO animateDelay
            putMVar mv_thread tid

      listenerUp _ = do
        removeEventListener button "mouseout" (pure eventListenerMouseOut) True
        sync $ fireButtonPressed False
        case buttonType of
          UpButton -> fireButtonEventValue
          DownButton -> do
            _ <- maybe (return ()) killThread <$> tryTakeMVar mv_thread
            return ()
          DelayButton -> do
            em <- tryTakeMVar mv_thread
            case (em) of
                 Nothing -> fireButtonEventValue
                 Just pid -> killThread pid


  eventListenerDown <- eventListenerNew listenerDown
  eventListenerUp   <- eventListenerNew listenerUp

  addEventListener button "mousedown" (pure eventListenerDown) True
  addEventListener button "mouseup" (pure eventListenerUp) True

  let bButtonState = buttonState <$> bEnabled <*> bButtonPressed
  cButtonState <- sync $ listen (value bButtonState) $
                  setAttribute button "data-state" . show



  empty_div <- _createDivElement doc
  setClassName empty_div "EmptyButton"

  cLabel <- sync $ listen (value bLabel) $ \t -> do
      setTitle button t
      setTextContent inner_span . pure $ t
      _removeFromParentIfExists parent button
      _removeFromParentIfExists parent empty_div
      _ <- appendChild parent . pure $
           if T.null t
           then castToHTMLElement empty_div
           else castToHTMLElement button
      return ()

  return $ _Button # (eButtonClick, cLabel >> cButtonState)



