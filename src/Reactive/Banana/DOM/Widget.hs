{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Reactive.Banana.DOM.Widget
       ( ReactiveDom, CleanupHandler, IsWidget(..), IsEventWidget(..), WidgetInstance,
         mkWidget, removeWidget, fromWidgetInstance,
         widgetRoot, mkSubWidget,  registerCleanupIO, handleBehavior,
         rsFlipFlop, newBehavior
       ) where


import           Control.Lens
import           Control.Monad
import           Control.Monad.Writer
import           Data.Typeable
import           GHCJS.DOM.Element          (Element, setAttribute)
import           GHCJS.DOM.Node             (getParentNode, removeChild)
import           GHCJS.DOM.Types            (IsNode)
import           Reactive.Banana
import           Reactive.Banana.Frameworks

--- | The 'Monad' in which 'WidgetInstance' creation is enacted. Use 'registerCleanupIO'
---   to register cleanup handles. Use 'mkSubWidget' to create sub 'WidgetInstance's
type ReactiveDom a =  WriterT [CleanupHandler] MomentIO a

-- | a type class for all reactive widgets
class IsWidget w where
  data WidgetInput w :: *

  -- | Create and register widget 'w'. Returns the widget and its root 'Element'.
  --   Use 'mkSubWidget' to creat sub widgets.
  mkWidgetInstance :: (IsNode parent) =>
    parent -> WidgetInput w -> ReactiveDom (w, Element)

-- | A widget witch emits an 'Event'
class (IsWidget w) => IsEventWidget w where
  type WidgetEventType w :: *
  widgetEvent :: w -> Event (WidgetEventType w)

type CleanupHandler = MomentIO ()

-- | Represents an instance of an 'IsWidget'
newtype WidgetInstance w = WidgetInstance (w,  CleanupHandler, Element)
makePrisms ''WidgetInstance

-- | Register an 'IO' action as cleanup function in 'ReactiveDom'
registerCleanupIO :: IO () -> ReactiveDom ()
registerCleanupIO = tell . pure . liftIO

-- | Get the widget from a 'WidgetInstance'
fromWidgetInstance :: (IsWidget w) => WidgetInstance w -> w
fromWidgetInstance = view $ _WidgetInstance . _1

-- | Should deregister all event handlers, kill threads etc.
widgetCleanup :: (IsWidget w) => WidgetInstance w -> CleanupHandler
widgetCleanup = view $ _WidgetInstance . _2

-- | Get the root 'Element' of a 'WidgetInstance'
widgetRoot :: (IsWidget w) => WidgetInstance w -> Element
widgetRoot = view $ _WidgetInstance . _3

-- | Creates a new 'WidgetInstance' in the DOM
mkWidget :: (Typeable w, IsWidget w, IsNode parent) =>
            parent -> WidgetInput w -> MomentIO (WidgetInstance w)
mkWidget parent i = do
  (r, cs) <- runWriterT $ mkWidgetInstance parent i
  let widgetClass = mkWidgetClassName . show . typeOf . view _1 $ r
  liftIOLater . setAttribute (r ^. _2) ("data-widget" :: String) $ widgetClass
  return $ _WidgetInstance # (r ^. _1, sequence_ cs, r ^. _2)

  where mkWidgetClassName = takeWhile (not . (==) ' ')

-- | Calls cleanup of 'WidgetInstance' tree. Removes element from DOM.
removeWidget :: (IsWidget w) => WidgetInstance w -> MomentIO ()
removeWidget w = do
   widgetCleanup w
   let r = widgetRoot w
   pM <- getParentNode r
   case pM of
     Nothing -> return ()
     Just p -> void $ removeChild p (pure r)

-- | Creates a sub widget in the 'ReactiveDom' 'Monad'.
mkSubWidget :: (Typeable w, IsWidget w, IsNode parent) =>
            parent -> WidgetInput w -> ReactiveDom (WidgetInstance w)
mkSubWidget parent i = do
 w <- lift . mkWidget parent $ i
 tell . pure . widgetCleanup $ w
 return w


newBehavior :: a -> MomentIO (Behavior a, a -> IO ())
newBehavior s0 = do
  (e, fe) <- newEvent
  (,) <$> stepper s0 e <*> pure fe


-- | register an handler for changes on a 'Behavior'
handleBehavior :: Behavior a -> Handler a -> ReactiveDom ()
handleBehavior a = lift . handleBehavior' a

handleBehavior' :: Behavior a -> Handler a -> MomentIO ()
handleBehavior' a h = do
  valueBLater a >>= liftIOLater . h
  changes a >>= reactimate' . fmap (fmap h)



rsFlipFlop :: Behavior Bool -> Behavior Bool -> MomentIO (Behavior Bool)
rsFlipFlop s r = do
  (b, fire) <- newBehavior False
  handleBehavior' s $ \a -> when a $ fire True
  handleBehavior' r $ \a -> when a $ fire False
  return b
