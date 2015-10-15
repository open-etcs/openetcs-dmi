
module Reactive.Banana.DOM
       ( IsWidget, IsEventWidget, widgetEvent,
         WidgetInstance, mkWidget, removeWidget,
         fromWidgetInstance,
         -- * Mouse Events
         registerMouseClick, registerMouseDown, registerMouseUp, registerMouseOut
       ) where

import           Control.Monad.Writer
import           GHCJS.DOM.EventTarget         (addEventListener,
                                                removeEventListener)
import           GHCJS.DOM.EventTargetClosures (eventListenerNew)
import           GHCJS.DOM.Types               (IsEvent, IsNode, MouseEvent)
import           Reactive.Banana
import           Reactive.Banana.DOM.Widget
import           Reactive.Banana.Frameworks


-- | see 'registerEvent'
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







