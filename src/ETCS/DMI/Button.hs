module ETCS.DMI.Button (
    Button, mkButton, buttonE, buttonCleanup
) where

import           Data.Maybe
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           ETCS.DMI.Types
import           FRP.Sodium
import           GHCJS.DOM.Document            (createElement)
import           GHCJS.DOM.Element             (setClassName)
import           GHCJS.DOM.EventTarget         (addEventListener)
import           GHCJS.DOM.EventTargetClosures (eventListenerNew)
import           GHCJS.DOM.HTMLButtonElement   as Button
import           GHCJS.DOM.HTMLElement         (setTitle)
import           GHCJS.DOM.Node                (appendChild, getFirstChild,
                                                hasChildNodes, removeChild,
                                                setTextContent)
import           GHCJS.DOM.Types               (IsDocument, IsNode, MouseEvent,
                                                castToHTMLDivElement,
                                                castToHTMLSpanElement)



mkButton :: (IsDocument d, IsNode p) => d -> p -> (Behavior Text, Behavior Bool) -> e
            -> IO (Button e)
mkButton doc parent (bLabel, bEnabled) eValue = do

  b <- fmap (castToHTMLButtonElement . fromMaybe (error "unable to create button")) $
       createElement doc $ pure "button"
  _ <- appendChild parent (pure b)

  sp <- fmap (castToHTMLSpanElement . fromMaybe (error "unable to span button")) $
       createElement doc $ pure ("span" :: String)
  empty_div <-
    fmap (castToHTMLDivElement . fromMaybe (error "unable to empty button div")) $
    createElement doc $ pure "div"
  setClassName empty_div "EmptyButton"

  (e, fe, cleanup) <- sync $ do
    cLabel <- listen (value bLabel) $ \t -> do
      setTitle b t
      cs <- hasChildNodes b
      setTextContent sp . pure $ t
      c0 <- getFirstChild b

      case (cs, T.null t) of
        (False, False) -> do
          _ <- appendChild b $ pure sp ; return ()
        (False, True)  -> do
          _ <- appendChild b $ pure empty_div ; return ()
        (True, False) -> do
          _ <- removeChild b c0
          _ <- appendChild b $ pure sp ; return ()
        (True, True)  -> do
          _ <- removeChild b c0
          _ <- appendChild b $ pure empty_div ; return ()



    cVisible <- listen (value bEnabled) $ Button.setDisabled b . not
    (e, fe) <- newEvent
    return (e, fe, cLabel >> cVisible)

  let listener :: MouseEvent -> IO ()
      listener _ = sync $ fe eValue
  eventListener <- eventListenerNew listener
  addEventListener b "click" (pure eventListener) False

  return $ Button {
    _buttonE = e,
    _buttonCleanup = cleanup
    }
