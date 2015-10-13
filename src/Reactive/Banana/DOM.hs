{-# LANGUAGE TypeFamilies #-}

module Reactive.Banana.DOM
       ( IsWidget(..), mkWidget, removeWidget, IsEventWidget(..),
         -- * Mouse Events
         registerMouseClick, registerMouseDown, registerMouseUp, registerMouseOut,
         focusBehavior, registerFocusInEvent, registerFocusOutEvent,
         -- * re-exports
         IsNode
       ) where

import           Data.Typeable
import           GHCJS.DOM.Element             (Element, setAttribute)
import           GHCJS.DOM.EventTarget         (addEventListener,
                                                removeEventListener)
import           GHCJS.DOM.EventTargetClosures (eventListenerNew)
import           GHCJS.DOM.Node                (getParentNode, removeChild)
import           GHCJS.DOM.Types               (FocusEvent, IsEvent, IsNode,
                                                MouseEvent)
import           Reactive.Banana
import           Reactive.Banana.Frameworks


registerMouseDown, registerMouseUp, registerMouseOut, registerMouseClick ::
  (IsNode n ) => n -> MomentIO (Event (), MomentIO ())
registerMouseDown   = registerMouseEvent "mousedown"
registerMouseUp     = registerMouseEvent "mouseup"
registerMouseOut    = registerMouseEvent "mouseout"
registerMouseClick  = registerMouseEvent "click"


registerMouseEvent :: (IsNode n) => String -> n -> MomentIO (Event (), MomentIO ())
registerMouseEvent = registerEvent (const () :: MouseEvent -> ())


registerFocusInEvent :: (IsNode n) => n -> MomentIO (Event (), MomentIO ())
registerFocusInEvent = registerEvent (const () :: FocusEvent -> ()) "focus"

registerFocusOutEvent :: (IsNode n) => n -> MomentIO (Event (), MomentIO ())
registerFocusOutEvent = registerEvent (const () :: FocusEvent -> ()) "blur"

focusBehavior :: (IsNode n) => n -> MomentIO (Behavior Bool, MomentIO ())
focusBehavior n = do
  (fi, cfi) <- registerFocusInEvent n
  (fo, cfo) <- registerFocusOutEvent n
  let e = unionWith const (fmap (const False) fo) (fmap (const True) fi)
  b <- stepper False e
  return (b, do cfi ; cfo)



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


class IsWidget w where
  data WidgetInput w :: *

  mkWidgetIO :: (IsNode parent) => parent -> WidgetInput w -> MomentIO w
  widgetRoot :: w -> Element
  widgetCleanup :: w -> MomentIO ()

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



class (IsWidget w) => IsEventWidget w where
  type WidgetEventType w :: *
  widgetEvent :: w -> Event (WidgetEventType w)



