{-# LANGUAGE TypeFamilies #-}

module Reactive.Banana.DOM
       ( IsWidget(..), mkWidget, removeWidget, IsEventWidget(..),
         -- * Mouse Events
         registerMouseClick, registerMouseDown, registerMouseUp, registerMouseOut,
         -- * re-exports
         IsNode
       ) where

import           Data.Typeable
import           GHCJS.DOM.Element             (Element, setAttribute)
import           GHCJS.DOM.EventTarget         (addEventListener,
                                                removeEventListener)
import           GHCJS.DOM.EventTargetClosures (eventListenerNew)
import           GHCJS.DOM.Node                (getParentNode, removeChild)
import           GHCJS.DOM.Types               (IsEvent, IsNode, MouseEvent)
import           Reactive.Banana
import           Reactive.Banana.Frameworks

-- | see 'registerEvent'
registerMouseDown, registerMouseUp, registerMouseOut, registerMouseClick ::
  (IsNode n ) => n -> MomentIO (Event (), MomentIO ())
registerMouseDown   = registerMouseEvent "mousedown"
registerMouseUp     = registerMouseEvent "mouseup"
registerMouseOut    = registerMouseEvent "mouseout"
registerMouseClick  = registerMouseEvent "click"

-- | register a named 'MouseEvent'. return reactive 'Event' and cleanup action.
registerMouseEvent :: (IsNode n) => String -> n -> MomentIO (Event (), MomentIO ())
registerMouseEvent = registerEvent (const () :: MouseEvent -> ())

-- | register an named 'EventListener' in DOM.
--   return it as an 'Event' and a cleanup action.
registerEvent ::
  (IsNode n, IsEvent e) => (e -> a) -> String -> n -> MomentIO (Event a, MomentIO ())
registerEvent h e t = do
  (ah, el) <- liftIO $ do
    (addHandler, fire) <- newAddHandler
    eventListener <- eventListenerNew (fire . h)
    addEventListener t e (pure eventListener) True
    return (addHandler, eventListener)
  ev <- fromAddHandler ah
  return (ev, liftIO $ removeEventListener t e (pure el) True)


-- | a type class for all reactive widgets
class IsWidget w where
  data WidgetInput w :: *

  -- | create and register 'Widget'. should not be called directly. use 'mkWidget'.
  mkWidgetIO :: (IsNode parent) => parent -> WidgetInput w -> MomentIO w

  -- | the base element of the 'Widget' in DOM.
  widgetRoot :: w -> Element

  -- | should deregister all event handlers, kill threads etc.
  widgetCleanup :: w -> MomentIO ()

-- | creates a new 'Widget' in the DOM
mkWidget :: (Typeable w, IsWidget w, IsNode parent) =>
            parent -> WidgetInput w -> MomentIO w
mkWidget parent i = do
  widget <- mkWidgetIO parent i
  let widgetClass = takeWhile (not . (==) ' ') . show . typeOf $ widget
  liftIOLater . setAttribute (widgetRoot widget) "data-widget" $ widgetClass
  return widget

-- | calls cleanup of 'Widget' tree. removes element from DOM
removeWidget :: (IsWidget w) => w -> MomentIO ()
removeWidget w = do
   widgetCleanup w
   let r = widgetRoot w
   pM <- getParentNode r
   case pM of
     Nothing -> return ()
     Just p -> removeChild p (pure r) >> return ()

-- | a 'Widget' witch emits an 'Event'
class (IsWidget w) => IsEventWidget w where
  type WidgetEventType w :: *
  widgetEvent :: w -> Event (WidgetEventType w)



