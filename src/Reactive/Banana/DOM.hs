{-# LANGUAGE TypeFamilies #-}

module Reactive.Banana.DOM
       ( IsWidget(..), mkWidget, IsEventWidget(..),
         -- * Mouse Events
         registerMouseDown, registerMouseUp, registerMouseOut,
         -- * re-exports
         IsNode
       ) where

import           Control.Monad
import           Data.Proxy
import           Data.Typeable
import           GHCJS.DOM.Element             (Element, setAttribute)
import           GHCJS.DOM.EventTarget         (addEventListener)
import           GHCJS.DOM.EventTargetClosures (eventListenerNew)
import           GHCJS.DOM.Types               (IsElement, IsNode, MouseEvent)
import           Reactive.Banana
import           Reactive.Banana.Frameworks

registerMouseDown, registerMouseUp, registerMouseOut ::
  (IsNode n ) => n -> MomentIO (Event ())
registerMouseDown n = liftIO (registerMouseEvent "mousedown" n) >>= fromAddHandler
registerMouseUp   n = liftIO (registerMouseEvent "mouseup" n) >>= fromAddHandler
registerMouseOut  n = liftIO (registerMouseEvent "mouseout" n) >>= fromAddHandler


registerMouseEvent :: (IsNode n) => String -> n -> IO (AddHandler ())
registerMouseEvent e t = do
  (addHandler, fire) <- newAddHandler
  let handler :: MouseEvent -> IO ()
      handler = const $ fire ()
  eventListener <- eventListenerNew handler
  addEventListener t e (pure eventListener) True
  return addHandler

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


