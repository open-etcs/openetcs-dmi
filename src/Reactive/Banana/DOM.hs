{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Reactive.Banana.DOM
       ( IsWidget(..), Widget, mkWidget, removeWidget,
         ReactiveDom, mkWidget', registerCleanupIO, IsEventWidget(..), widgetWidget,
         -- * Mouse Events
         registerMouseClick, registerMouseDown, registerMouseUp, registerMouseOut,
         -- * re-exports
         IsNode
       ) where


import           Control.Monad.Writer
import           Data.Typeable
import           GHCJS.DOM.Element             (Element, setAttribute)
import           GHCJS.DOM.EventTarget         (addEventListener,
                                                removeEventListener)
import           GHCJS.DOM.EventTargetClosures (eventListenerNew)
import           GHCJS.DOM.Node                (getParentNode, removeChild)
import           GHCJS.DOM.Types               (IsEvent, IsNode, MouseEvent)
import           Reactive.Banana
import           Reactive.Banana.Frameworks


type ReactiveDom a =  WriterT [MomentIO ()] MomentIO a


-- | see 'registerEvent'
registerMouseDown, registerMouseUp, registerMouseOut, registerMouseClick ::
  (IsNode n ) => n -> ReactiveDom (Event ())
registerMouseDown   = registerMouseEvent "mousedown"
registerMouseUp     = registerMouseEvent "mouseup"
registerMouseOut    = registerMouseEvent "mouseout"
registerMouseClick  = registerMouseEvent "click"

-- | register a named 'MouseEvent'. return reactive 'Event' and cleanup action.
registerMouseEvent :: (IsNode n) => String -> n -> ReactiveDom (Event ())
registerMouseEvent = registerEvent (const () :: MouseEvent -> ())



-- | register an named 'EventListener' in DOM.
--   return it as an 'Event' and a cleanup action.
registerEvent ::
  (IsNode n, IsEvent e) => (e -> a) -> String -> n -> ReactiveDom (Event a)
registerEvent h e t = do
  (ah, el) <- liftIO $ do
    (addHandler, fire) <- newAddHandler
    eventListener <- eventListenerNew (fire . h)
    addEventListener t e (pure eventListener) True
    return (addHandler, eventListener)
  tell [ liftIO $ removeEventListener t e (pure el) True ]
  lift $ fromAddHandler ah

--  return (ev, liftIO $ removeEventListener t e (pure el) True )


-- | a type class for all reactive widgets

newtype Widget w = Widget (w,  MomentIO ())


class IsWidget w where
  data WidgetInput w :: *

  -- | create and register 'Widget'. should not be called directly. use 'mkWidget'.
  mkWidgetIO :: (IsNode parent) => parent -> WidgetInput w -> ReactiveDom w

  -- | the base element of the 'Widget' in DOM.
  widgetRoot :: w -> Element

registerCleanupIO :: IO () -> ReactiveDom ()
registerCleanupIO = tell . pure . liftIO

-- | should deregister all event handlers, kill threads etc.
widgetCleanup :: Widget w -> MomentIO ()
widgetCleanup (Widget w) = snd w

widgetWidget :: Widget w -> w
widgetWidget (Widget w) = fst w

-- | creates a new 'Widget' in the DOM
mkWidget :: (Typeable w, IsWidget w, IsNode parent) =>
            parent -> WidgetInput w -> MomentIO (Widget w)
mkWidget parent i = do
  (widget, cs) <- runWriterT $ mkWidgetIO parent i
  let widgetClass = takeWhile (not . (==) ' ') . show . typeOf $ widget
  liftIOLater . setAttribute (widgetRoot widget) "data-widget" $ widgetClass
  return $ Widget (widget, sequence cs >> return ())

mkWidget' :: (Typeable w, IsWidget w, IsNode parent) =>
            parent -> WidgetInput w -> ReactiveDom (Widget w)
mkWidget' parent i = do
 w <- lift $ mkWidget parent i
 tell . pure . widgetCleanup $ w
 return w


-- | calls cleanup of 'Widget' tree. removes element from DOM
removeWidget :: (IsWidget w) => Widget w -> MomentIO ()
removeWidget w = do
   widgetCleanup w
   let r = widgetRoot . widgetWidget $ w
   pM <- getParentNode r
   case pM of
     Nothing -> return ()
     Just p -> removeChild p (pure r) >> return ()

-- | a 'Widget' witch emits an 'Event'
class (IsWidget w) => IsEventWidget w where
  type WidgetEventType w :: *
  widgetEvent :: w -> Event (WidgetEventType w)



