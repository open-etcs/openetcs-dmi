
module Reactive.Banana.DOM
       ( IsWidget, IsEventWidget, widgetEvent,
         WidgetInstance, mkWidget, removeWidget,
         fromWidgetInstance,
         -- * Mouse Events
         registerMouseClick, registerMouseDown, registerMouseUp, registerMouseOut,
         -- * Utils
         deleteChildNodes
       ) where

import           Control.Monad.Writer
import           GHCJS.DOM.EventTarget         (addEventListener,
                                                removeEventListener)
import           GHCJS.DOM.EventTargetClosures (eventListenerNew)
import           GHCJS.DOM.Node                (getFirstChild, removeChild)
import           GHCJS.DOM.Types               (IsEvent, IsNode, MouseEvent)
import           Reactive.Banana
import           Reactive.Banana.DOM.Widget
import           Reactive.Banana.Frameworks


--- | Register mouse click 'Event'.
registerMouseDown, registerMouseUp, registerMouseOut, registerMouseClick ::
  (IsNode n ) => n -> ReactiveDom (Event ())
registerMouseDown   = registerMouseEvent "mousedown"
registerMouseUp     = registerMouseEvent "mouseup"
registerMouseOut    = registerMouseEvent "mouseout"
registerMouseClick  = registerMouseEvent "click"

-- | register a named 'MouseEvent'. return reactive 'Event'.
registerMouseEvent :: (IsNode n) => String -> n -> ReactiveDom (Event ())
registerMouseEvent = registerEvent (const () :: MouseEvent -> ())

-- | register an named 'EventListener' in DOM.
--   return it as an 'Event'.
registerEvent ::
  (IsNode n, IsEvent e) => (e -> a) -> String -> n -> ReactiveDom (Event a)
registerEvent h e t = do
  (ah, el) <- liftIO $ do
    (addHandler, fire) <- newAddHandler
    eventListener <- eventListenerNew (fire . h)
    addEventListener t e (pure eventListener) True
    return (addHandler, eventListener)
  registerCleanupIO $ removeEventListener t e (pure el) True
  lift $ fromAddHandler ah



deleteChildNodes :: (MonadIO m, IsNode n) => n -> m ()
deleteChildNodes p = do
  c <- getFirstChild p
  case c of
    Nothing -> return ()
    Just n -> removeChild p (pure n) >> deleteChildNodes p





