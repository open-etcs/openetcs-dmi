{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Reactive.Banana.DOM
       ( IsWidget(..), IsEventWidget(..),
         WidgetInstance, mkWidget, removeWidget, widgetRoot,
         ReactiveDom, mkSubWidget, registerCleanupIO, fromWidgetInstance,
         -- * Mouse Events
         registerMouseClick, registerMouseDown, registerMouseUp, registerMouseOut,
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

-- | a type class for all reactive widgets
class IsWidget w where
  data WidgetInput w :: *

  -- | Create and register widget 'w'. Returns the widget and its root 'Element'.
  --   Use 'mkSubWidget' to creat sub widgets.
  mkWidgetInstance :: (IsNode parent) =>
    parent -> WidgetInput w -> ReactiveDom (w, Element)

-- | a widget witch emits an 'Event'
class (IsWidget w) => IsEventWidget w where
  type WidgetEventType w :: *
  widgetEvent :: w -> Event (WidgetEventType w)

-- | represents an instance of an 'IsWidget'
newtype WidgetInstance w = WidgetInstance (w,  MomentIO (), Element)

-- | the monad in which 'WidgetInstance' creation is enacted. use 'registerCleanupIO'
--   to register cleanup handles. use 'mkSubWidget to create sub 'WidgetInstance's
type ReactiveDom a =  WriterT [MomentIO ()] MomentIO a



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

-- | register an 'IO' action as cleanup function in 'ReactiveDom'
registerCleanupIO :: IO () -> ReactiveDom ()
registerCleanupIO = tell . pure . liftIO

-- | should deregister all event handlers, kill threads etc.
widgetCleanup :: (IsWidget w) => WidgetInstance w -> MomentIO ()
widgetCleanup (WidgetInstance (_, cs, _)) = cs

-- | get the 'IsWidget' from a WidgetInstance
fromWidgetInstance :: (IsWidget w) => WidgetInstance w -> w
fromWidgetInstance (WidgetInstance (w, _, _)) = w

-- | get the root 'Element' of a 'WidgetInstance'
widgetRoot :: (IsWidget w) => WidgetInstance w -> Element
widgetRoot (WidgetInstance (_, _, e)) = e

-- | creates a new 'WidgetInstance' in the DOM
mkWidget :: (Typeable w, IsWidget w, IsNode parent) =>
            parent -> WidgetInput w -> MomentIO (WidgetInstance w)
mkWidget parent i = do
  ((widget, e), cs) <- runWriterT $ mkWidgetInstance parent i
  let widgetClass = takeWhile (not . (==) ' ') . show . typeOf $ widget
  liftIOLater . setAttribute e "data-widget" $ widgetClass
  return $ WidgetInstance (widget, sequence cs >> return (), e)


-- | calls cleanup of 'WidgetInstance' tree. removes element from DOM
removeWidget :: (IsWidget w) => WidgetInstance w -> MomentIO ()
removeWidget w = do
   widgetCleanup w
   let r = widgetRoot w
   pM <- getParentNode r
   case pM of
     Nothing -> return ()
     Just p -> removeChild p (pure r) >> return ()

-- | creates a sub widget in the 'ReactiveDom' 'Monad'.
mkSubWidget :: (Typeable w, IsWidget w, IsNode parent) =>
            parent -> WidgetInput w -> ReactiveDom (WidgetInstance w)
mkSubWidget parent i = do
 w <- lift $ mkWidget parent i
 tell . pure . widgetCleanup $ w
 return w






