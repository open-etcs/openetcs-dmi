module ETCS.DMI.Button (
    Button, mkButton, buttonE, buttonCleanup
) where


import           Control.Lens
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           ETCS.DMI.Helpers
import           ETCS.DMI.Types
import           FRP.Sodium
import           GHCJS.DOM.Element             (setAttribute, setClassName)
import           GHCJS.DOM.EventTarget         (addEventListener)
import           GHCJS.DOM.EventTargetClosures (eventListenerNew)
import           GHCJS.DOM.HTMLButtonElement   as Button
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
            (Behavior Text, Behavior Bool) -> e -> IO (Button e)
mkButton doc parent (bLabel, bEnabled) buttonEventValue = do
  button <- _createDivElement doc
  setAttribute button "data-role" "button"
  inner_div <- _createDivElement doc
  inner_span <- _createSpanElement doc
  _ <- appendChild inner_div $ pure inner_span
  _ <- appendChild button $ pure inner_div

  (bButtonPressed, fireButtonPressed) <- sync $ newBehavior False

  let listener :: Bool -> MouseEvent -> IO ()
      listener a _ = do
        print a
        sync $ fireButtonPressed a

  eventListenerDown <- eventListenerNew $ listener True
  eventListenerUp   <- eventListenerNew $ listener False

  addEventListener button "mousedown" (pure eventListenerDown) True
  addEventListener button "mouseup" (pure eventListenerUp) True

  let bButtonState = buttonState <$> bEnabled <*> bButtonPressed
  cButtonState <- sync $ listen (value bButtonState) $
                  setAttribute button "data-state" . show

  (eButtonClick, fireButtonClick) <- sync newEvent


  empty_div <- _createDivElement doc
  setClassName empty_div "EmptyButton"

  cLabel <- sync $ do
    listen (value bLabel) $ \t -> do
      setTitle button t
      setTextContent inner_span . pure $ t
      _removeFromParentIfExists parent button
      _removeFromParentIfExists parent empty_div
      _ <- appendChild parent . pure $
           if (T.null t)
           then castToHTMLElement empty_div
           else castToHTMLElement button
      return ()

  return $ _Button # (eButtonClick, cLabel >> cButtonState)



