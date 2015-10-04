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
import           GHCJS.DOM.Node                (appendChild, getFirstChild,
                                                hasChildNodes, removeChild,
                                                setTextContent)
import           GHCJS.DOM.Types               (IsDocument, IsNode, MouseEvent)

import           Control.Lens

mkButton :: (IsDocument d, IsNode p) => d -> p ->
            (Behavior Text, Behavior Bool) -> e -> IO (Button e)
mkButton doc parent (bLabel, bEnabled) eValue = do

  button <- _createButtonElement doc
  () <$ (appendChild parent $ pure button)

  inner_span <- _createSpanElement doc
  empty_div <- _createDivElement doc
  setClassName empty_div "EmptyButton"

  (eButtonClick, fireButtonClick, cleanup) <- sync $ do
    cLabel <- listen (value bLabel) $ \t -> do
      setTitle button t
      cs <- hasChildNodes button
      setTextContent inner_span . pure $ t
      c0 <- getFirstChild button

      case (cs, T.null t) of
        (False, False) -> do
          _ <- appendChild button $ pure inner_span ; return ()
        (False, True)  -> do
          _ <- appendChild button $ pure empty_div ; return ()
        (True, False) -> do
          _ <- removeChild button c0
          _ <- appendChild button $ pure inner_span ; return ()
        (True, True)  -> do
          _ <- removeChild button c0
          _ <- appendChild button $ pure empty_div ; return ()



    cVisible <- listen (value bEnabled) $ Button.setDisabled button . not
    (eButtonClick, fireButtonClick) <- newEvent
    return (eButtonClick, fireButtonClick, cLabel >> cVisible)

  let listener :: MouseEvent -> IO ()
      listener = const . sync $ fireButtonClick eValue
  eventListener <- eventListenerNew listener
  addEventListener button "click" (pure eventListener) False

  return $ _Button # (eButtonClick, cleanup)
