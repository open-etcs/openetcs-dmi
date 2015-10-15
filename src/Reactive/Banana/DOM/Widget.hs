{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Reactive.Banana.DOM.Widget
       ( ReactiveDom, CleanupHandler, IsWidget(..), IsEventWidget(..), WidgetInstance,
         mkWidget, removeWidget, fromWidgetInstance,
         widgetRoot, mkSubWidget,  registerCleanupIO
       ) where


import           Control.Lens
import           Control.Monad.Writer
import           Data.Typeable
import           GHCJS.DOM.Element          (Element, setAttribute)



import           GHCJS.DOM.Node             (getParentNode, removeChild)
import           GHCJS.DOM.Types            (IsNode)
import           Reactive.Banana
import           Reactive.Banana.Frameworks


-- | the monad in which 'WidgetInstance' creation is enacted. use 'registerCleanupIO'
--   to register cleanup handles. use 'mkSubWidget to create sub 'WidgetInstance's
type ReactiveDom a =  WriterT [CleanupHandler] MomentIO a

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

type CleanupHandler = MomentIO ()

-- | represents an instance of an 'IsWidget'
newtype WidgetInstance w = WidgetInstance (w,  CleanupHandler, Element)
makePrisms ''WidgetInstance

-- | register an 'IO' action as cleanup function in 'ReactiveDom'
registerCleanupIO :: IO () -> ReactiveDom ()
registerCleanupIO = tell . pure . liftIO

-- | get the 'IsWidget' from a WidgetInstance
fromWidgetInstance :: (IsWidget w) => WidgetInstance w -> w
fromWidgetInstance = view $ _WidgetInstance . _1

-- | should deregister all event handlers, kill threads etc.
widgetCleanup :: (IsWidget w) => WidgetInstance w -> CleanupHandler
widgetCleanup = view $ _WidgetInstance . _2

-- | get the root 'Element' of a 'WidgetInstance'
widgetRoot :: (IsWidget w) => WidgetInstance w -> Element
widgetRoot = view $ _WidgetInstance . _3

-- | creates a new 'WidgetInstance' in the DOM
mkWidget :: (Typeable w, IsWidget w, IsNode parent) =>
            parent -> WidgetInput w -> MomentIO (WidgetInstance w)
mkWidget parent i = do
  (r, cs) <- runWriterT $ mkWidgetInstance parent i
  let widgetClass = mkWidgetClassName . show . typeOf . view _1 $ r
  liftIOLater . setAttribute (r ^. _2) "data-widget" $ widgetClass
  return $ _WidgetInstance # (r ^. _1, sequence cs >> return (), r ^. _2)

  where mkWidgetClassName = takeWhile (not . (==) ' ')

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


