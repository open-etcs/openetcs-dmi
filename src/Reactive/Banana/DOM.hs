{-# LANGUAGE TypeFamilies #-}

module Reactive.Banana.DOM
       ( IsWidget(..), mkWidget, IsEventWidget(..),
         -- * Mouse Events
         registerMouseClick, registerMouseDown, registerMouseUp, registerMouseOut,
         focusBehavior, registerFocusInEvent, registerFocusOutEvent,
         -- * re-exports
         IsNode
       ) where

import           Data.Typeable
import           GHCJS.DOM.Element             (Element, setAttribute)
import           GHCJS.DOM.EventTarget         (addEventListener)
import           GHCJS.DOM.EventTargetClosures (eventListenerNew)
import           GHCJS.DOM.Types               (FocusEvent, IsEvent, IsNode,
                                                MouseEvent)
import           Reactive.Banana
import           Reactive.Banana.Frameworks


registerMouseDown, registerMouseUp, registerMouseOut, registerMouseClick ::
  (IsNode n ) => n -> MomentIO (Event ())
registerMouseDown   = registerMouseEvent "mousedown"
registerMouseUp     = registerMouseEvent "mouseup"
registerMouseOut    = registerMouseEvent "mouseout"
registerMouseClick  = registerMouseEvent "click"


registerMouseEvent :: (IsNode n) => String -> n -> MomentIO (Event ())
registerMouseEvent = registerEvent (const () :: MouseEvent -> ())


registerFocusInEvent :: (IsNode n) => n -> MomentIO (Event ())
registerFocusInEvent = registerEvent (const () :: FocusEvent -> ()) "focus"

registerFocusOutEvent :: (IsNode n) => n -> MomentIO (Event ())
registerFocusOutEvent = registerEvent (const () :: FocusEvent -> ()) "blur"

focusBehavior :: (IsNode n) => n -> MomentIO (Behavior Bool)
focusBehavior n = do
  fi <- registerFocusInEvent n
  fo <- registerFocusOutEvent n
  let e = unionWith const (fmap (const False) fo) (fmap (const True) fi)
  stepper False e



registerEvent ::
  (IsNode n, IsEvent e) => (e -> a) -> String -> n -> MomentIO (Event a)
registerEvent h e t = do
  ah <- liftIO $ do
    (addHandler, fire) <- newAddHandler
    eventListener <- eventListenerNew (fire . h)
    addEventListener t e (pure eventListener) True
    return addHandler
  fromAddHandler ah


class IsWidget w where
  data WidgetInput w :: *

  mkWidgetIO :: (MonadIO m, IsNode parent) =>
               parent -> WidgetInput w -> m (MomentIO w, Element)


mkWidget :: (Typeable w, IsWidget w, IsNode parent) =>
            parent -> WidgetInput w -> MomentIO w
mkWidget parent i = do
  (widgetM, node) <- mkWidgetIO parent i
  widget <- widgetM
  let widgetClass = takeWhile (not . (==) ' ') . show . typeOf $ widget
  liftIOLater . setAttribute node "data-widget" $ widgetClass
  return widget



class (IsWidget w) => IsEventWidget w where
  type WidgetEventType w :: *
  widgetEvent :: w -> Event (WidgetEventType w)


