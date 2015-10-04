module ETCS.DMI.Button (
    Button, ButtonType (..), mkButton, buttonE, buttonCleanup
) where


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

  let mouseOutHandler :: MouseEvent -> IO ()
      mouseOutHandler _ = do
        print "out"
        sync $ fireButtonPressed False
  eventListenerMouseOut <- eventListenerNew mouseOutHandler

  (eButtonClick, fireButtonClick) <- sync newEvent

  let listenerDown, listenerUp :: MouseEvent -> IO ()
      listenerDown _ = do
        print "down"
        addEventListener button "mouseout" (pure eventListenerMouseOut) True
        sync $ fireButtonPressed True

      listenerUp _ = do
        print "up"
        removeEventListener button "mouseout" (pure eventListenerMouseOut) True
        sync $ fireButtonPressed False
        if (buttonType == UpButton)
          then sync $ fireButtonClick buttonEventValue
          else return ()




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



