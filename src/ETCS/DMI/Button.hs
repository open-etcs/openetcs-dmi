module ETCS.DMI.Button (
    Button, mkButton, buttonE, buttonCleanup
) where


import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           ETCS.DMI.Helpers
import           ETCS.DMI.Types
import           FRP.Sodium
import           GHCJS.DOM.Element             (setClassName)
import           GHCJS.DOM.EventTarget         (addEventListener)
import           GHCJS.DOM.EventTargetClosures (eventListenerNew)
import           GHCJS.DOM.HTMLButtonElement   as Button
import           GHCJS.DOM.HTMLElement         (setTitle)
import           GHCJS.DOM.Node                (appendChild, setTextContent)
import           GHCJS.DOM.Types               (IsDocument, IsNode, MouseEvent,
                                                castToHTMLElement)

import           Control.Lens

mkButton :: (IsDocument d, IsNode p, Show e) => d -> p ->
            (Behavior Text, Behavior Bool) -> e -> IO (Button e)
mkButton doc parent (bLabel, bEnabled) buttonEventValue = do
  button <- _createButtonElement doc
  inner_span <- _createSpanElement doc
  _ <- appendChild button $ pure inner_span
  cVisible <- sync $ listen (value bEnabled) $ Button.setDisabled button . not

  (eButtonClick, fireButtonClick) <- sync newEvent
  let listener :: MouseEvent -> IO ()
      listener = const . sync $ fireButtonClick buttonEventValue
  eventListener <- eventListenerNew listener
  addEventListener button "click" (pure eventListener) False


  --() <$ (appendChild parent $ pure button)

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

  return $ _Button # (eButtonClick, cLabel >> cVisible)



